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
path_opt     <- file.path(getwd(), "Data", "成分股_RDS")
path_output  <- file.path(getwd(), "Output_CBXM")
path_vix_csv <- "C:/Users/User/Desktop/Thesis_project/data_raw/VIX_CBXM.csv"

if (!dir.exists(path_output)) dir.create(path_output, recursive = TRUE)

# 參數設定
# 可更換為 "DD", "MDLZ", "RTX"
target_ticker <- "DD" 
moneyness      <- 1.00
start_date    <- as.Date("2004-08-31")
end_date      <- as.Date("2023-08-31")
strike_mult   <- 1000

# [CUSIP 映射] 
special_cusip_map <- list(
  "DD(OLD)" = "26353410", "AA" = "44320110", "MTLQ" = c("62010A10", "37044210"),
  "EK" = "27746110", "T(OLD)" = "00195750", "DD" = "26614N10", "RTX" = "75513E10"
)

# 3. 輔助函數 =====
get_dji_price <- function(target_date, dji_df) {
  target_date <- as.Date(target_date)
  subset <- dji_df[as.Date(dji_df$Date) <= target_date, ]
  if (nrow(subset) == 0) return(NA)
  return(tail(subset$Close, 1))
}

get_target_rf <- function(trade_date, rf_data){
  target_rf <- rf_data %>% 
    filter(as.Date(date) >= trade_date) %>% 
    arrange(date, abs(days - 30)) %>% 
    slice(1) %>% pull(rate)
  if(length(target_rf) == 0) 0 else target_rf
}

get_stock_row_fast <- function(stock_df, target_date, window_before = 1, window_after = 2) {
  hit <- stock_df %>% filter(date == target_date)
  if (nrow(hit) > 0) return(hit[1, , drop = FALSE])
  hit2 <- stock_df %>%
    filter(date >= (target_date - days(window_before)) & date <= (target_date + days(window_after))) %>%
    head(1)
  return(if(nrow(hit2) > 0) hit2[1, , drop = FALSE] else NULL)
}

# 4. 讀取基礎資料 (SQL & VIX) =====
message(paste("正在執行 CBXM 特殊案件合併處理:", target_ticker))
con <- dbConnect(RSQLite::SQLite(), path_db)

div_raw_df <- dbReadTable(con, "dividend_history") %>%
  mutate(ex_date = as.Date(ex_date), amount = suppressWarnings(as.numeric(amount))) %>%
  filter(!is.na(ex_date), !is.na(amount), amount > 0)

rf_raw <- dbReadTable(con, "rf_rate") %>%
  mutate(date = as.Date(date), days = suppressWarnings(as.numeric(days)), rate = suppressWarnings(as.numeric(rate)))

dji_index_df <- dbReadTable(con, "dji_index") %>%
  mutate(Date = as.Date(Date), Close = suppressWarnings(as.numeric(Close))) %>% arrange(Date)

# VIX 讀取邏輯
vix_signals <- read.csv(path_vix_csv, check.names = FALSE, stringsAsFactors = FALSE)
names(vix_signals)[1] <- "Execution_Date"
vix_signals <- vix_signals %>% mutate(Execution_Date = as.Date(Execution_Date))

# 5. 建立全局交易日曆 =====
month_seq <- seq(floor_date(start_date, "month"), floor_date(end_date, "month"), by = "month")
raw_dates <- as.Date(sapply(month_seq, function(d) {
  days <- seq(floor_date(d, "month"), ceiling_date(d, "month") - days(1), by = "day")
  fridays <- days[lubridate::wday(days) == 6]
  if(length(fridays) >= 3) return(fridays[3]) else return(NA)
}), origin = "1970-01-01")

nyse_holidays   <- as.Date(holidayNYSE(year = year(start_date):year(end_date)))
roll_dates      <- if_else(raw_dates %in% nyse_holidays, raw_dates - days(1), raw_dates)
schedule_global <- data.frame(Date_Roll = roll_dates, Date_Expiry = lead(roll_dates)) %>% na.omit()

# 6. 執行回測 (整合 DD, MDLZ, RTX 邏輯) =====
tryCatch({
  # [A] 成分股身分判定
  target_opt_row <- dbGetQuery(con, "SELECT * FROM dji_constituents_raw WHERE ticker = ?", params = list(target_ticker))
  equity_cols    <- names(target_opt_row)[grepl("%", names(target_opt_row))]
  status_records <- data.frame(Report_Date = as.Date(character()), Is_Member = logical())
  month_map      <- c("Jan"="01","Feb"="02","Mar"="03","Apr"="04","May"="05","Jun"="06",
                      "Jul"="07","Aug"="08","Sep"="09","Oct"="10","Nov"="11","Dec"="12")
  
  for (col in equity_cols) {
    val_num <- suppressWarnings(as.numeric(target_opt_row[[col]]))
    if (!is.na(val_num)) {
      clean_col <- gsub(" % Of Equity", "", col); clean_col <- gsub("\\.", "-", clean_col)
      parts <- strsplit(clean_col, "-")[[1]]
      if (length(parts) >= 3) {
        m_str <- month_map[parts[1]]; date_str <- paste0(parts[3], "-", m_str, "-", parts[2])
        status_records <- rbind(status_records, data.frame(Report_Date = as.Date(date_str), Is_Member = (val_num > 0)))
      }
    }
  }
  status_records <- status_records %>% arrange(Report_Date)
  valid_indices <- which(sapply(schedule_global$Date_Roll, function(d) {
    rep <- status_records[status_records$Report_Date <= d, ]; if(nrow(rep) > 0) tail(rep, 1)$Is_Member else FALSE
  }))
  schedule <- schedule_global[valid_indices, ]
  
  # [B] 股價與股利
  stock_df <- dbGetQuery(con, "SELECT date, open, close, cfadj FROM stock_prices WHERE ticker = ?", params = list(target_ticker)) %>%
    mutate(date = as.Date(date), open = abs(as.numeric(open)), close = abs(as.numeric(close)), cfadj = as.numeric(cfadj)) %>%
    arrange(date)
  target_divs <- div_raw_df %>% filter(ticker == target_ticker)
  
  # [C] 執行核心迴圈
  results_list <- list()
  rds_cache <- list()
  
  for (i in 1:nrow(schedule)) {
    curr_date <- schedule$Date_Roll[i]; next_date <- schedule$Date_Expiry[i]
    
    # VIX Signal 判定
    vix_info <- vix_signals %>% filter(Execution_Date == curr_date)
    sig_val  <- if(nrow(vix_info) > 0) vix_info$Is_High_Vol else 0
    
    stock_T <- get_stock_row_fast(stock_df, curr_date)
    if(is.null(stock_T)) next
    
    S_close_T <- stock_T$close; S_open_T <- stock_T$open; cfadj_T <- if(!is.na(stock_T$cfadj)) stock_T$cfadj else 1
    curr_rf <- get_target_rf(curr_date, rf_raw)
    
    strike_selected <- NA; premium_received <- 0; status_msg <- "VIX Low: Buy & Hold"
    
    # CBXM 邏輯：高波動才賣權
    if (sig_val == 1) {
      file_pattern <- format(curr_date, "%Y%m")
      if (is.null(rds_cache[[file_pattern]])) {
        rds_f <- list.files(path_opt, pattern = paste0("^", file_pattern, ".*\\.rds$"), full.names = TRUE)
        if (length(rds_f) > 0) rds_cache[[file_pattern]] <- readRDS(rds_f[1])
      }
      opt_raw <- rds_cache[[file_pattern]]
      if(!is.null(opt_raw)) {
        use_date <- max(opt_raw$temp_date[as.Date(opt_raw$temp_date) <= curr_date], na.rm=T)
        target_opts <- opt_raw %>% filter(as.Date(temp_date) == as.Date(use_date), cp_flag == "C", abs(as.numeric(as.Date(exdate) - next_date)) <= 2)
        
        if (target_ticker %in% names(special_cusip_map)) {
          target_opts <- target_opts %>% filter(cusip %in% special_cusip_map[[target_ticker]])
        } else {
          target_opts <- target_opts %>% filter(ticker == target_ticker)
        }
        
        if(nrow(target_opts) > 0) {
          target_opts <- target_opts %>% mutate(real_strike = strike_price / strike_mult)
          candidates  <- target_opts %>% filter(real_strike >= S_close_T)
          if(nrow(candidates) > 0) {
            selected <- candidates %>% mutate(diff = abs(real_strike - S_close_T * moneyness)) %>% arrange(diff) %>% slice(1)
            strike_selected <- selected$real_strike; premium_received <- as.numeric(selected$best_bid)
            status_msg <- "VIX High: Sell Call"
          }
        }
      }
    }
    
    # [D] 結算與合併特殊處理邏輯
    stock_Next <- get_stock_row_fast(stock_df, next_date, window_after = 2)
    cbxm_ret <- NA; bnh_ret <- NA; market_ret <- NA; payoff_val <- 0; total_div_val <- 0
    S_close_Next_Adj <- NA
    
    if(!is.null(stock_Next)) {
      # 合併分拆補償邏輯
      is_special_month <- FALSE
      adj_multiplier   <- 1.0
      
      if (target_ticker == "DD" && curr_date == as.Date("2019-03-15")) {
        adj_multiplier <- 1.5035
        is_special_month <- TRUE
      } else if (target_ticker == "MDLZ" && curr_date == as.Date("2012-09-21")) {
        adj_multiplier <- 1.5405
        is_special_month <- TRUE
      } else if (target_ticker == "RTX" && curr_date == as.Date("2020-03-20")) {
        adj_multiplier <- 1.5890
        is_special_month <- TRUE
      }
      
      adj_ratio_cf     <- cfadj_T / stock_Next$cfadj
      S_close_Next_Adj <- stock_Next$close * adj_multiplier
      
      # 特殊月排除股利
      if (!is_special_month) {
        total_div_val <- sum(target_divs %>% filter(ex_date > curr_date, ex_date <= next_date) %>% pull(amount)) * adj_ratio_cf
      }
      
      # 結算計算
      cost_basis <- S_close_T - premium_received
      if(!is.na(strike_selected)) {
        payoff_val <- max(0, S_close_Next_Adj - strike_selected)
        if(payoff_val > 0) status_msg <- "VIX High: Exercised (ITM)" else status_msg <- "VIX High: Expired (OTM)"
        cbxm_ret <- ((S_close_Next_Adj + total_div_val - payoff_val) / cost_basis) - 1
      } else {
        # VIX 低波動時即為標的持股報酬
        cbxm_ret <- ((S_close_Next_Adj + total_div_val) / S_close_T) - 1
      }
      bnh_ret <- ((S_close_Next_Adj + total_div_val) / S_close_T) - 1
    }    
    
    dji_T <- get_dji_price(curr_date, dji_index_df); dji_Next <- get_dji_price(next_date, dji_index_df)
    market_ret <- if(!is.na(dji_T) && !is.na(dji_Next)) (dji_Next / dji_T) - 1 else 0
    
    # 欄位對接 (CBXM 專屬 VIX_Signal 欄位)
    results_list[[i]] <- data.frame(
      Trading_Date=curr_date, Expiry_Date=next_date, Ticker=target_ticker, VIX_Signal=sig_val,
      Moneyness=moneyness, Split=cfadj_T, Open_T=S_open_T, Close_T=S_close_T, Strike=strike_selected, 
      Premium=premium_received, Close_T1=round(S_close_Next_Adj, 4), Dividend=round(total_div_val,4), 
      Payoff=-payoff_val, Status=status_msg, RiskFreeRate_30D=curr_rf,
      CBXM_Return=ifelse(is.na(cbxm_ret),0,round(cbxm_ret,6)),
      BnH_Return=ifelse(is.na(bnh_ret),0,round(bnh_ret,6)), Market_Return=round(market_ret,6)
    )
  }  
  
  # [E] 績效分析與產出 (CBXM 專屬色彩: goldenrod, blue, gray)
  df_final <- do.call(rbind, results_list)
  if(!is.null(df_final) && nrow(df_final) > 0) {
    ts_data  <- xts(df_final[, c("CBXM_Return", "BnH_Return", "Market_Return")], order.by = df_final$Expiry_Date)
    col_names <- c(paste0("CBXM (M=", moneyness, ")"), paste0("Buy & Hold (", target_ticker, ")"), "Market (DJITR)")
    colnames(ts_data) <- col_names
    ts_ret_plot <- rbind(xts(matrix(0,1,3), order.by=min(df_final$Trading_Date)), ts_data)
    colnames(ts_ret_plot) <- col_names
    ts_rf <- xts(df_final$RiskFreeRate_30D/1200, order.by=df_final$Expiry_Date)
    
    tab_ann <- table.AnnualizedReturns(ts_data[,1:2], Rf=ts_rf, scale=12)
    max_dd <- maxDrawdown(ts_data[,1:2]); sortino <- SortinoRatio(ts_data[,1:2], MAR=0)
    calmar <- CalmarRatio(ts_data[,1:2], scale=12); treynor <- TreynorRatio(Ra=ts_data[,1:2], Rb=ts_data[,3], Rf=ts_rf, scale=12)
    ir <- InformationRatio(Ra=ts_data[,1:2], Rb=ts_data[,3], scale=12)
    beta_cbxm <- CAPM.beta(Ra=ts_data[,1], Rb=ts_data[,3], Rf=ts_rf); beta_stock <- CAPM.beta(Ra=ts_data[,2], Rb=ts_data[,3], Rf=ts_rf)
    
    perf_matrix <- rbind(tab_ann, max_dd, sortino, calmar, "Beta (vs DJITR)"=c(beta_cbxm, beta_stock), "Treynor Ratio"=treynor, "Information Ratio"=ir)
    df_perf <- cbind(Metric = rownames(perf_matrix), as.data.frame(perf_matrix))
    
    temp_plot <- paste0(path_output, "/chart_", target_ticker, ".png")
    png(temp_plot, width = 1200, height = 800, res = 120)
    # 嚴格維持 CBXM 專屬顏色
    charts.PerformanceSummary(ts_ret_plot, main = paste("CBXM Strategy Performance:", target_ticker), wealth.index = TRUE, colorset = c("goldenrod", "blue", "gray"), lwd = 2)
    dev.off()
    
    wb <- createWorkbook(); addWorksheet(wb, "Trade_Log"); writeData(wb, "Trade_Log", df_final)
    addWorksheet(wb, "Performance"); writeData(wb, "Performance", df_perf)
    addWorksheet(wb, "Chart"); insertImage(wb, "Chart", temp_plot, width = 10, height = 6, startRow = 2, startCol = 2)
    saveWorkbook(wb, paste0(path_output, "/CBXM_", target_ticker, ".xlsx"), overwrite = TRUE)
    if(file.exists(temp_plot)) file.remove(temp_plot)
    message(paste(target_ticker, "CBXM 整合處理完成。檔案儲存於:", path_output))
  }
}, error = function(e) message(paste("Error processing", target_ticker, ":", e$message)))

dbDisconnect(con)