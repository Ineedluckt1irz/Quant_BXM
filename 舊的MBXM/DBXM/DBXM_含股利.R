# ==============================================================================
# DBXM (Dual-Condition Buy-Write Model) - Perfect Benchmark Version
# 只有在「恐慌期 (CBXM)」+「弱勢股 (MBXM)」時才賣個股 Call
# ==============================================================================

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
path_db        <- "C:/Users/User/Desktop/Thesis_project/Data/SQLDB1.sqlite"
path_opt       <- file.path(getwd(), "Data", "成分股_RDS")
path_output    <- file.path(getwd(), "Output_DBXM")
path_vix_csv   <- "C:/Users/User/Desktop/Thesis_project/data_raw/VIX_CBXM.csv"
path_rank_csv  <- "C:/Users/User/Desktop/Thesis_project/Output_MBXM/MBXM_Rank_90d_Top10.xlsx"

if (!dir.exists(path_output)) dir.create(path_output, recursive = TRUE)

# 參數設定
moneyness   <- 1.00
start_date  <- as.Date("2004-08-31")
end_date    <- as.Date("2023-08-31")
strike_mult <- 1000

# 特殊標的 CUSIP 映射表
special_cusip_map <- list(
  "DD(OLD)" = "26353410", "AA" = "44320110", "MTLQ" = c("62010A10", "37044210"),
  "EK" = "27746110", "T(OLD)" = "00195750", "DD" = "26614N10", "RTX" = "75513E10"
)

# 3. 輔助函數定義 =====
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

# 4. 讀取基礎資料 (SQL, VIX, Rank) =====
message("正在載入 DBXM 雙因子基準資料...")
con <- dbConnect(RSQLite::SQLite(), path_db)

div_raw_df <- dbReadTable(con, "dividend_history") %>%
  mutate(ex_date = as.Date(ex_date), amount = suppressWarnings(as.numeric(amount))) %>%
  filter(!is.na(ex_date), !is.na(amount), amount > 0)

rf_raw <- dbReadTable(con, "rf_rate") %>%
  mutate(date = as.Date(date), days = as.numeric(days), rate = as.numeric(rate))

dji_index_df <- dbReadTable(con, "dji_index") %>%
  mutate(Date = as.Date(Date), Close = as.numeric(Close)) %>% arrange(Date)

# 載入 VIX 信號
vix_signals <- read.csv(path_vix_csv, check.names = FALSE, stringsAsFactors = FALSE)
names(vix_signals)[1] <- "Execution_Date"
vix_signals <- vix_signals %>% mutate(Execution_Date = as.Date(Execution_Date))

# 載入動能排名 (日期校正)
rank_data <- read.xlsx(path_rank_csv) %>%
  mutate(Trade_Date = as.Date(as.numeric(Trade_Date), origin = "1899-12-30"), Ticker = toupper(trimws(Ticker)))

all_tickers <- dbGetQuery(con, "SELECT DISTINCT ticker FROM dji_constituents_raw") %>% pull(ticker) %>% sort()

# 5. 建立全局交易日曆 =====
month_seq <- seq(floor_date(start_date, "month"), floor_date(end_date, "month"), by = "month")
raw_dates <- as.Date(sapply(month_seq, function(d) {
  days <- seq(floor_date(d, "month"), ceiling_date(d, "month") - days(1), by = "day")
  fridays <- days[lubridate::wday(days) == 6]
  if(length(fridays) >= 3) return(fridays[3]) else return(NA)
}), origin = "1970-01-01")

nyse_holidays <- as.Date(holidayNYSE(year = year(start_date):year(end_date)))
roll_dates    <- if_else(raw_dates %in% nyse_holidays, raw_dates - days(1), raw_dates)
schedule_global <- data.frame(Date_Roll = roll_dates, Date_Expiry = lead(roll_dates)) %>% na.omit()

rds_cache <- list()

# 6. 執行量產迴圈 =====
success_count <- 0
pb <- txtProgressBar(min = 0, max = length(all_tickers), style = 3)

for (idx in seq_along(all_tickers)) {
  test_ticker <- all_tickers[idx]
  setTxtProgressBar(pb, idx)
  
  tryCatch({
    # [A] 成分股身分判定
    target_opt_row <- dbGetQuery(con, "SELECT * FROM dji_constituents_raw WHERE ticker = ?", params = list(test_ticker))
    if(nrow(target_opt_row) == 0) next
    
    # (解析身分區間... 同步標準版邏輯)
    equity_cols <- names(target_opt_row)[grepl("%", names(target_opt_row))]
    status_records <- data.frame(Report_Date = as.Date(character()), Is_Member = logical())
    month_map <- c("Jan"="01","Feb"="02","Mar"="03","Apr"="04","May"="05","Jun"="06",
                   "Jul"="07","Aug"="08","Sep"="09","Oct"="10","Nov"="11","Dec"="12")
    for (col in equity_cols) {
      val_num <- suppressWarnings(as.numeric(target_opt_row[[col]]))
      if (!is.na(val_num)) {
        clean_col <- gsub(" % Of Equity", "", col); clean_col <- gsub("\\.", "-", clean_col)
        parts <- strsplit(clean_col, "-")[[1]]
        if (length(parts) >= 3 && parts[1] %in% names(month_map)) {
          date_str <- paste0(parts[3], "-", month_map[parts[1]], "-", parts[2])
          status_records <- rbind(status_records, data.frame(Report_Date = as.Date(date_str), Is_Member = (val_num > 0)))
        }
      }
    }
    status_records <- status_records %>% arrange(Report_Date)
    valid_indices <- which(sapply(schedule_global$Date_Roll, function(d) {
      rep <- status_records[status_records$Report_Date <= d, ]; if(nrow(rep) > 0) tail(rep, 1)$Is_Member else FALSE
    }))
    if(length(valid_indices) == 0) next
    schedule <- schedule_global[valid_indices, ]
    
    # [B] 股價讀取
    stock_df <- dbGetQuery(con, "SELECT date, open, close, cfadj FROM stock_prices WHERE ticker = ?", params = list(test_ticker)) %>%
      mutate(date = as.Date(date), open = abs(as.numeric(open)), close = abs(as.numeric(close)), cfadj = as.numeric(cfadj)) %>%
      filter(!is.na(open), open > 0) %>% arrange(date)
    target_divs <- div_raw_df %>% filter(ticker == test_ticker)
    
    # [C] 核心迴圈
    results_list <- list()
    for (i in 1:nrow(schedule)) {
      curr_date <- schedule$Date_Roll[i]; next_date <- schedule$Date_Expiry[i]
      
      # 1. 取得 VIX 信號 (CBXM 因子)
      vix_info <- vix_signals %>% filter(Execution_Date == curr_date)
      vix_sig  <- if(nrow(vix_info) > 0) vix_info$Is_High_Vol else 0
      
      # 2. 取得動能排名 (MBXM 因子)
      current_mom_info <- rank_data %>% 
        filter(Ticker == test_ticker) %>%
        mutate(diff = abs(as.numeric(Trade_Date - curr_date))) %>%
        filter(diff <= 3) %>% arrange(diff) %>% slice(1)
      is_strong <- if(nrow(current_mom_info) > 0) as.numeric(current_mom_info$Is_Strong_Stock[1]) else 0
      
      stock_T <- get_stock_row_fast(stock_df, curr_date)
      if(is.null(stock_T)) next
      S_close_T <- stock_T$close; S_open_T <- stock_T$open
      cfadj_T   <- if(!is.na(stock_T$cfadj)) stock_T$cfadj else 1
      curr_rf   <- get_target_rf(curr_date, rf_raw)
      
      strike_selected <- NA; premium_received <- 0
      status_msg <- "DBXM: Buy & Hold (Condition Not Met)"
      
      # 3. DBXM 決策核心：Panic (vix_sig=1) AND Weak Stock (is_strong=0)
      if (vix_sig == 1 && is_strong == 0) {
        file_pattern <- format(curr_date, "%Y%m")
        if (is.null(rds_cache[[file_pattern]])) {
          rds_f <- list.files(path_opt, pattern = paste0("^", file_pattern, ".*\\.rds$"), full.names = TRUE)
          if (length(rds_f) > 0) rds_cache[[file_pattern]] <- tryCatch(readRDS(rds_f[1]), error=function(e)NULL)
        }
        opt_raw <- rds_cache[[file_pattern]]
        if(!is.null(opt_raw)) {
          use_date <- max(opt_raw$temp_date[as.Date(opt_raw$temp_date) <= curr_date & as.Date(opt_raw$temp_date) >= (curr_date - 10)], na.rm=T)
          target_opts <- opt_raw %>% filter(as.Date(temp_date) == as.Date(use_date), cp_flag == "C", abs(as.numeric(as.Date(exdate) - next_date)) <= 2)
          if (test_ticker %in% names(special_cusip_map)) target_opts <- target_opts %>% filter(cusip %in% special_cusip_map[[test_ticker]]) else target_opts <- target_opts %>% filter(ticker == test_ticker)
          
          if(nrow(target_opts) > 0) {
            target_opts <- target_opts %>% mutate(real_strike = strike_price / strike_mult)
            candidates  <- target_opts %>% filter(real_strike >= S_close_T)
            if(nrow(candidates) > 0) {
              selected <- candidates %>% mutate(diff = abs(real_strike - S_close_T * moneyness)) %>% arrange(diff) %>% slice(1)
              strike_selected <- selected$real_strike; premium_received <- as.numeric(selected$best_bid)
              status_msg <- "DBXM: Sell Call (Panic + Weak)"
            }
          }
        }
      }
      
      # [D] 結算
      stock_Next <- get_stock_row_fast(stock_df, next_date)
      dbxm_ret <- NA; bnh_ret <- NA; payoff_val <- 0; total_div_val <- 0; S_close_Next_Adj <- NA
      if(!is.null(stock_Next)) {
        adj_ratio <- cfadj_T / stock_Next$cfadj
        S_close_Next_Adj <- stock_Next$close * adj_ratio
        total_div_val <- sum(target_divs %>% filter(ex_date > curr_date, ex_date <= next_date) %>% pull(amount)) * adj_ratio
        cost_basis <- S_close_T - premium_received
        if(!is.na(strike_selected)) {
          payoff_val <- max(0, S_close_Next_Adj - strike_selected)
          dbxm_ret <- ((S_close_Next_Adj + total_div_val - payoff_val) / cost_basis) - 1
        } else {
          dbxm_ret <- ((S_close_Next_Adj + total_div_val) / S_close_T) - 1
        }
        bnh_ret <- ((S_close_Next_Adj + total_div_val) / S_close_T) - 1
      }
      dji_T <- get_dji_price(curr_date, dji_index_df); dji_Next <- get_dji_price(next_date, dji_index_df)
      market_ret <- if(!is.na(dji_T) && !is.na(dji_Next)) (dji_Next/dji_T)-1 else 0
      
      results_list[[i]] <- data.frame(
        Trading_Date=curr_date, Expiry_Date=next_date, Ticker=test_ticker, 
        VIX_Signal=vix_sig, Is_Strong=is_strong,
        Moneyness=moneyness, Split=cfadj_T, Close_T=S_close_T, Strike=strike_selected, 
        Premium=premium_received, Close_T1=round(S_close_Next_Adj, 4), Dividend=round(total_div_val,4), 
        Payoff=-payoff_val, Status=status_msg, RiskFreeRate_30D=curr_rf,
        DBXM_Return=round(ifelse(is.na(dbxm_ret),0,dbxm_ret),6),
        BnH_Return=round(ifelse(is.na(bnh_ret),0,bnh_ret),6), Market_Return=round(market_ret,6)
      )
    }
    
    # [E] 績效分析與產出 (同步完美格式) =====
    df_final <- do.call(rbind, results_list)
    if(!is.null(df_final) && nrow(df_final) > 0) {
      ts_data  <- xts(df_final[, c("DBXM_Return", "BnH_Return", "Market_Return")], order.by = df_final$Expiry_Date)
      col_names <- c("DBXM (Dual-Cond)", paste0("Buy & Hold (", test_ticker, ")"), "Market (DJITR)")
      colnames(ts_data) <- col_names
      ts_ret_plot <- rbind(xts(matrix(0,1,3), order.by=min(df_final$Trading_Date)), ts_data)
      colnames(ts_ret_plot) <- col_names
      ts_rf <- xts(df_final$RiskFreeRate_30D/1200, order.by=df_final$Expiry_Date)
      
      tab_ann <- table.AnnualizedReturns(ts_data[,1:2], Rf=ts_rf, scale=12)
      max_dd <- maxDrawdown(ts_data[,1:2]); sortino <- SortinoRatio(ts_data[,1:2], MAR=0)
      calmar <- CalmarRatio(ts_data[,1:2], scale=12); treynor <- TreynorRatio(Ra=ts_data[,1:2], Rb=ts_data[,3], Rf=ts_rf, scale=12)
      ir <- InformationRatio(Ra=ts_data[,1:2], Rb=ts_data[,3], scale=12)
      beta_dbxm <- CAPM.beta(Ra=ts_data[,1], Rb=ts_data[,3], Rf=ts_rf); beta_stock <- CAPM.beta(Ra=ts_data[,2], Rb=ts_data[,3], Rf=ts_rf)
      
      perf_matrix <- rbind(tab_ann, max_dd, sortino, calmar, "Beta (vs DJITR)"=c(beta_dbxm, beta_stock), "Treynor Ratio"=treynor, "Information Ratio"=ir)
      df_perf <- cbind(Metric = rownames(perf_matrix), as.data.frame(perf_matrix))
      
      temp_plot <- paste0(path_output, "/chart_", test_ticker, ".png")
      png(temp_plot, width = 1200, height = 800, res = 120)
      charts.PerformanceSummary(ts_ret_plot, main = paste("Strategy Performance (DBXM):", test_ticker), wealth.index = TRUE, colorset = c("purple", "blue", "gray"), lwd = 2)
      dev.off()
      
      wb <- createWorkbook(); addWorksheet(wb, "Trade_Log"); writeData(wb, "Trade_Log", df_final)
      addWorksheet(wb, "Performance"); writeData(wb, "Performance", df_perf)
      addWorksheet(wb, "Chart"); insertImage(wb, "Chart", temp_plot, width = 10, height = 6, startRow = 2, startCol = 2)
      saveWorkbook(wb, paste0(path_output, "/DBXM_", test_ticker, ".xlsx"), overwrite = TRUE)
      if(file.exists(temp_plot)) file.remove(temp_plot)
      success_count <- success_count + 1
    }
  }, error = function(e) message(paste("\nError:", test_ticker, ":", e$message)))
}

close(pb); dbDisconnect(con)
message(paste("\nDBXM 雙因子量產完成。成功處理總數:", success_count))