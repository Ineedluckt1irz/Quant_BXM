# 1. 套件載入 =====
library(RSQLite)
library(openxlsx)
library(dplyr)
library(lubridate)
library(timeDate)
library(PerformanceAnalytics)
library(xts)
library(readxl)
library(zoo)

# 2. 設定區 =====
path_db      <- "C:/Users/User/Desktop/Thesis_project/Data/SQLDB1.sqlite"
path_opt_dia <- "C:/Users/User/Desktop/Thesis_project/data_raw/DIA_opt"
path_vix_csv <- "C:/Users/User/Desktop/Thesis_project/data_raw/VIX_CBXM.csv"
path_output  <- file.path(getwd(), "Output_IBXM")

if (!dir.exists(path_output)) dir.create(path_output, recursive = TRUE)

# 參數設定
moneyness     <- 1.00
start_date    <- as.Date("2004-08-31")
end_date      <- as.Date("2023-08-31")
strike_mult   <- 1000 
target_ticker <- "DIA"

# 3. 輔助函數定義 =====
get_target_rf <- function(trade_date, rf_data){
  target_rf <- rf_data %>% 
    filter(as.Date(date) >= trade_date) %>% 
    arrange(date, abs(days - 30)) %>% 
    slice(1) %>% pull(rate)
  if(length(target_rf) == 0) 0 else target_rf
}

get_stock_row_fast <- function(stock_df, target_date, window_before = 2, window_after = 2) {
  date_col <- if("Date" %in% names(stock_df)) "Date" else "date"
  hit <- stock_df[stock_df[[date_col]] == target_date, ]
  if (nrow(hit) > 0) return(hit[1, , drop = FALSE])
  hit2 <- stock_df %>% filter(!!sym(date_col) >= (target_date - days(window_before)) & !!sym(date_col) <= (target_date + days(window_after))) %>% head(1)
  return(if(nrow(hit2) > 0) hit2[1, , drop = FALSE] else NULL)
}

# 4. 讀取基礎資料 =====
con <- dbConnect(RSQLite::SQLite(), path_db)

dia_df <- dbReadTable(con, "dia_index") %>% mutate(Date = as.Date(Date), Close = as.numeric(Close)) %>% arrange(Date)
dji_index_df <- dbReadTable(con, "dji_index") %>% mutate(Date = as.Date(Date), Close = as.numeric(Close)) %>% arrange(Date)
div_raw_df <- dbReadTable(con, "dividend_history") %>% filter(ticker == target_ticker) %>% mutate(ex_date = as.Date(ex_date), amount = as.numeric(amount))
rf_raw <- dbReadTable(con, "rf_rate") %>% mutate(date = as.Date(date), days = as.numeric(days), rate = as.numeric(rate))

# 讀取 VIX 信號
vix_signals  <- read.csv(path_vix_csv, check.names = FALSE, stringsAsFactors = FALSE)
names(vix_signals)[1] <- "Execution_Date"
vix_signals <- vix_signals %>% mutate(Execution_Date = as.Date(Execution_Date))

# 5. 建立交易日曆 =====
month_seq <- seq(floor_date(start_date, "month"), floor_date(end_date, "month"), by = "month")
raw_dates <- as.Date(sapply(month_seq, function(d) {
  days <- seq(floor_date(d, "month"), ceiling_date(d, "month") - days(1), by = "day")
  fridays <- days[lubridate::wday(days) == 6]; if(length(fridays) >= 3) return(fridays[3]) else return(NA)
}), origin = "1970-01-01")

nyse_holidays <- as.Date(holidayNYSE(year = year(start_date):year(end_date)))
roll_dates    <- if_else(raw_dates %in% nyse_holidays, raw_dates - days(1), raw_dates)
schedule      <- data.frame(Date_Roll = roll_dates, Date_Expiry = lead(roll_dates)) %>% na.omit()

# 6. 回測主迴圈 =====
rds_cache <- list(); results_list <- list()

for (i in 1:nrow(schedule)) {
  curr_date <- schedule$Date_Roll[i]; next_date <- schedule$Date_Expiry[i]
  
  # VIX Signal 判斷 (1 = 高波動賣 Call, 0 = 低波動純持股)
  vix_info <- vix_signals %>% filter(Execution_Date == curr_date)
  sig_val  <- if(nrow(vix_info) > 0) vix_info$Is_High_Vol else 0
  
  stock_T <- get_stock_row_fast(dia_df, curr_date)
  if(is.null(stock_T)) next
  S_close_T <- stock_T$Close; curr_rf <- get_target_rf(curr_date, rf_raw)
  
  # 讀取選擇權
  file_pattern <- format(curr_date, "%Y%m")
  rds_path <- file.path(path_opt_dia, paste0(file_pattern, "_DIA.rds"))
  strike_selected <- NA; premium_received <- 0
  
  if (file.exists(rds_path)) {
    if (is.null(rds_cache[[file_pattern]])) rds_cache[[file_pattern]] <- readRDS(rds_path)
    opt_raw <- rds_cache[[file_pattern]]; names(opt_raw) <- toupper(names(opt_raw))
    target_opts <- opt_raw %>% filter(CP_FLAG == "C", abs(as.numeric(as.Date(EXDATE) - next_date)) <= 2, as.Date(DATE) == curr_date)
    
    if(nrow(target_opts) > 0) {
      target_opts <- target_opts %>% mutate(real_strike = STRIKE_PRICE / strike_mult)
      selected <- target_opts %>% mutate(diff = abs(real_strike - S_close_T * moneyness)) %>% arrange(diff) %>% slice(1)
      strike_selected  <- selected$real_strike; premium_received <- as.numeric(selected$BEST_BID)
    }
  }
  
  # 結算與報酬計算
  stock_Next <- get_stock_row_fast(dia_df, next_date)
  ibxm_ret <- NA; cibxm_ret <- NA; bnh_ret <- NA; market_ret <- 0; payoff_val <- 0; total_div_val <- 0
  
  if(!is.null(stock_Next)) {
    S_close_Next <- stock_Next$Close
    total_div_val <- sum(div_raw_df %>% filter(ex_date > curr_date, ex_date <= next_date) %>% pull(amount))
    
    # 計算 IBXM (固定賣 Call) 的報酬
    if(!is.na(strike_selected)) {
      payoff_val <- max(0, S_close_Next - strike_selected)
      cost_basis <- S_close_T - premium_received
      ibxm_ret   <- ((S_close_Next + total_div_val - payoff_val) / cost_basis) - 1
    }
    
    # 計算 DIA B&H (純持股) 的報酬
    bnh_ret <- ((S_close_Next + total_div_val) / S_close_T) - 1
    
    # 核心邏輯：C-IBXM (條件式賣 Call)
    if (sig_val == 1 && !is.na(strike_selected)) {
      cibxm_ret <- ibxm_ret  # VIX 高，賣 Call
    } else {
      cibxm_ret <- bnh_ret   # VIX 低，純持股
    }
  }
  
  # 市場基準 (DJI)
  m_T <- get_stock_row_fast(dji_index_df, curr_date); m_Next <- get_stock_row_fast(dji_index_df, next_date)
  if(!is.null(m_T) && !is.null(m_Next)) market_ret <- (m_Next$Close / m_T$Close) - 1
  
  results_list[[i]] <- data.frame(
    Trading_Date=curr_date, Expiry_Date=next_date, VIX_Signal=sig_val, Strike=strike_selected, Premium=premium_received,
    IBXM_Return=round(ifelse(is.na(ibxm_ret), 0, ibxm_ret), 6),
    CIBXM_Return=round(ifelse(is.na(cibxm_ret), 0, cibxm_ret), 6),
    DIA_BnH_Return=round(ifelse(is.na(bnh_ret), 0, bnh_ret), 6),
    Market_Return=round(market_ret, 6), RiskFreeRate_30D=curr_rf
  )
}

# 7. 績效分析與產出 =====
df_final <- do.call(rbind, results_list) %>% filter(!is.na(IBXM_Return))

if(nrow(df_final) > 0) {
  # 注意：此處欄位名稱需與 results_list 定義完全一致
  ts_data <- xts(df_final[, c("IBXM_Return", "CIBXM_Return", "DIA_BnH_Return", "Market_Return")], order.by = df_final$Expiry_Date)
  col_names <- c(paste0("IBXM (M=", moneyness, ")"), "C-IBXM (VIX)", "DIA ETF", "Market (DJITR)")
  colnames(ts_data) <- col_names
  
  ts_rf <- xts(df_final$RiskFreeRate_30D/1200, order.by=df_final$Expiry_Date)
  
  # 補齊全部績效指標
  tab_ann <- table.AnnualizedReturns(ts_data[,1:3], Rf=ts_rf, scale=12)
  max_dd  <- maxDrawdown(ts_data[,1:3])
  sortino <- SortinoRatio(ts_data[,1:3], MAR=0)
  calmar  <- CalmarRatio(ts_data[,1:3], scale=12)
  treynor <- TreynorRatio(Ra=ts_data[,1:3], Rb=ts_data[,4], Rf=ts_rf, scale=12)
  ir      <- InformationRatio(Ra=ts_data[,1:3], Rb=ts_data[,4], scale=12)
  beta_ibxm  <- CAPM.beta(Ra=ts_data[,1], Rb=ts_data[,4], Rf=ts_rf)
  beta_cibxm <- CAPM.beta(Ra=ts_data[,2], Rb=ts_data[,4], Rf=ts_rf)
  beta_dia   <- CAPM.beta(Ra=ts_data[,3], Rb=ts_data[,4], Rf=ts_rf)
  
  perf_matrix <- rbind(tab_ann, max_dd, sortino, calmar, 
                       "Beta (vs DJITR)"=c(beta_ibxm, beta_cibxm, beta_dia), 
                       "Treynor Ratio"=treynor, "Information Ratio"=ir)
  df_perf <- cbind(Metric = rownames(perf_matrix), as.data.frame(perf_matrix))
  
  # 繪圖
  temp_plot <- paste0(path_output, "/chart_DIA_CIBXM.png")
  png(temp_plot, width = 1200, height = 800, res = 120)
  charts.PerformanceSummary(ts_data[,1:3], main = "Strategy Performance: IBXM", wealth.index = TRUE, colorset = c("red", "goldenrod", "blue"), lwd = 2)
  dev.off()
  
  # 儲存 Excel
  wb <- createWorkbook()
  addWorksheet(wb, "Trade_Log"); writeData(wb, "Trade_Log", df_final)
  addWorksheet(wb, "Performance"); writeData(wb, "Performance", df_perf)
  addWorksheet(wb, "Chart"); insertImage(wb, "Chart", temp_plot, width = 10, height = 6, startRow = 2, startCol = 2)
  
  saveWorkbook(wb, file.path(path_output, "IBXM_DIA.xlsx"), overwrite = TRUE)
  message("✅ IBXM 指數策略回測完成，報表已儲存。")
}

dbDisconnect(con)