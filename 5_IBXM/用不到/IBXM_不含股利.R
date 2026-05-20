# 1. 套件載入 =====
library(RSQLite)
library(openxlsx)
library(dplyr)
library(lubridate)
library(timeDate)
library(PerformanceAnalytics)
library(xts)
library(readxl) # 新增: 用於讀取 Excel 股價檔

# 2. 設定區 =====
current_path <- getwd()  

# 設定路徑
path_db     <- file.path(current_path, "Data", "SQLDB.sqlite")
# 改為 DIA 選擇權資料夾
path_opt    <- "C:/Users/User/Desktop/Thesis_project/data_raw/DIA_opt" 
# 設定 DIA 股價檔案路徑
path_dia_price <- "C:/Users/User/Desktop/Thesis_project/data_raw/DIA_還原.xls"
path_output <- file.path(current_path, "Output")

# 參數
moneyness   <- 1.00
start_date  <- as.Date("2019-01-01")
end_date    <- as.Date("2023-08-31")
strike_mult <- 1000

# 3. 讀取資料 =====
message("正在連線SQL資料庫...")
con <- dbConnect(RSQLite::SQLite(), path_db)

# [A] 設定回測標的 (改為單一標的 DIA)
all_tickers <- c("DIA") 

# [B] 無風險利率(RF)
rf_raw <- dbReadTable(con, "rf_rate") %>%
  mutate(date = as.Date(date), days = as.numeric(days), rate = as.numeric(rate)) %>%
  filter(!is.na(days), !is.na(rate))

get_target_rf <- function(trade_date, rf_data){
  target_rf <- rf_data %>%
    filter(date >= trade_date) %>%
    arrange(date, abs(days - 30)) %>%
    slice(1) %>%
    pull(rate)
  if(length(target_rf) == 0) 0 else target_rf
}

# [C] 大盤指數 (DJI - 作為績效比較基準)
dji_temp <- dbReadTable(con, "dji_index")
names(dji_temp) <- tolower(names(dji_temp))
dji_index_df <- dji_temp %>%
  mutate(Date = as.Date(date), Close = as.numeric(close)) %>%
  select(Date, Close) %>% arrange(Date)

get_dji_price <- function(target_date, dji_df) {  
  subset <- dji_df %>% filter(Date <= target_date)
  if (nrow(subset) == 0) return(NA)
  return(tail(subset, 1)$Close)
}

# [D] 產生交易日曆
month_seq <- seq(floor_date(start_date, "month"), floor_date(end_date, "month"), by = "month")
get_3rd_friday <- function(date_obj) {
  days <- seq(floor_date(date_obj, "month"), ceiling_date(date_obj, "month") - days(1), by = "day")
  fridays <- days[wday(days, label = TRUE) %in% c("Fri", "週五", "Fri.", "Friday")]
  if(length(fridays) >= 3) return(fridays[3]) else return(NA)
}
raw_dates <- as.Date(sapply(month_seq, get_3rd_friday), origin = "1970-01-01")
raw_dates <- raw_dates[!is.na(raw_dates)] 
nyse_holidays <- as.Date(holidayNYSE(year = year(start_date):year(end_date)))
roll_dates <- if_else(raw_dates %in% nyse_holidays, raw_dates - days(1), raw_dates)

schedule_global <- data.frame(Date_Roll = roll_dates, Date_Expiry = lead(roll_dates)) %>% 
  na.omit() %>% filter(Date_Roll >= start_date & Date_Roll <= end_date)

message(paste("準備開始執行 IBXM 策略 (Target: DIA)..."))

# 4. 跑迴圈  =====
success_count <- 0
fail_count <- 0
summary_list <- list()

pb_total <- txtProgressBar(min = 0, max = length(all_tickers), style = 3)

for (idx in seq_along(all_tickers)) {
  target_ticker <- all_tickers[idx]
  setTxtProgressBar(pb_total, idx)
  
  tryCatch({
    # [A] 設定有效期間 (IBXM 視為全程有效，不需檢查成分股狀態)
    schedule <- schedule_global
    if(nrow(schedule) == 0) stop("無有效期間")
    
    # [B] 讀取股價 (改為讀取 DIA.xls)
    # 假設 Excel 第一欄為 Date，第二欄為 Price (若只有單一價格，Open/Close 皆設為該價格)
    if (!file.exists(path_dia_price)) stop(paste("找不到檔案:", path_dia_price))
    
    raw_dia_data <- read_excel(path_dia_price)
    
    # 簡單清洗欄位 (依據使用者提供的 Excel 結構進行對應)
    # 這裡假設第1欄是日期, 第2欄是價格. 若有完整 OHLC 可自行修改索引
    stock_df <- raw_dia_data %>%
      select(1, 2) %>% 
      setNames(c("date", "close")) %>%
      mutate(
        date = as.Date(date), # 若 Excel 日期格式為數字，需視情況調整 origin，如 as.Date(date, origin="1899-12-30")
        close = as.numeric(close),
        open = close,  # 若無 Open 資料，暫以 Close 替代
        cfadj = 1      # ETF 調整因子預設為 1
      ) %>%
      arrange(date) %>%
      filter(!is.na(date), !is.na(close))
    
    if(nrow(stock_df) == 0) stop("DIA 股價資料讀取後為空")
    
    # [C] 執行回測迴圈
    results_list <- list()
    
    for (i in 1:nrow(schedule)) {
      curr_date <- schedule$Date_Roll[i]
      next_date <- schedule$Date_Expiry[i]
      
      stock_T <- stock_df %>% filter(date == curr_date)
      if(nrow(stock_T)==0) stock_T <- stock_df %>%
        filter(date >= curr_date - 1 & date <= curr_date + 1) %>%
        head(1)
      if(nrow(stock_T)==0) next
      
      S_close_T <- as.numeric(stock_T$close)
      S_open_T  <- as.numeric(stock_T$open)
      cfadj_T   <- if (!is.na(stock_T$cfadj[1])) stock_T$cfadj[1] else 1
      curr_rf   <- get_target_rf(curr_date, rf_raw)
      
      # 讀取RDS (改為讀取 YYYYMM_DIA.RDS)
      file_tag <- format(curr_date, "%Y%m")
      # 搜尋檔名包含 YYYYMM 且包含 DIA 的檔案
      rds_files <- list.files(path_opt, pattern = paste0(file_tag, ".*DIA.*\\.rds$"), full.names = TRUE, ignore.case = TRUE)
      
      opt_raw <- NULL; premium_received <- 0; strike_selected <- NA
      
      if(length(rds_files) > 0) {
        tryCatch({
          opt_raw <- readRDS(rds_files[1]) 
          if(!is.null(opt_raw$temp_date)) {
            valid_dates <- unique(opt_raw$temp_date[opt_raw$temp_date <= curr_date & opt_raw$temp_date >= (curr_date - 5)])
            if(length(valid_dates) > 0) {
              use_date <- max(valid_dates)
              # 在 DIA 資料中 ticker 可能標記為 DIA 或其他，這裡主要依靠檔案名稱正確，因此放寬 ticker 篩選或鎖定 "DIA"
              target_opts <- opt_raw %>% 
                filter(temp_date == use_date, as.Date(exdate) == next_date, 
                       cp_flag == "C") %>% # 這裡不強制 filter ticker == target_ticker，以免資料庫名稱大小寫不一致
                mutate(real_strike = strike_price / strike_mult)
              
              if ("contract_size" %in% names(opt_raw) && any(target_opts$contract_size == 100)) {
                target_opts <- target_opts %>%
                  filter(contract_size == 100)
              }
              
              target_strike_price <- S_close_T * moneyness
              candidates <- target_opts %>% filter(real_strike >= S_close_T) 
              
              if (nrow(candidates) > 0) {
                selected_opt <- candidates %>%
                  mutate(diff = abs(real_strike - target_strike_price)) %>%
                  arrange(diff) %>%
                  slice(1)
                strike_selected  <- selected_opt$real_strike
                premium_received <- as.numeric(selected_opt$best_bid)
              } else {
                backup_opt <- target_opts %>%
                  arrange(desc(real_strike)) %>%
                  slice(1)
                if(nrow(backup_opt) > 0) {
                  strike_selected  <- backup_opt$real_strike
                  premium_received <- as.numeric(backup_opt$best_bid)
                }
              }
            }
          }
        }, error = function(e) {})
      }
      
      # [D] 結算
      stock_Next <- stock_df %>%
        filter(date == next_date)
      if(nrow(stock_Next)==0) stock_Next <- stock_df %>%
        filter(date >= next_date-1 & date <= next_date+2) %>%
        head(1)
      
      sbxm_ret <- NA; bnh_ret <- NA; market_ret <- NA
      status_msg <- "資料不足"; payoff_val <- 0; S_open_Next <- NA
      
      if(nrow(stock_Next) > 0) {
        S_close_Next <- as.numeric(stock_Next$close)
        S_open_Next  <- as.numeric(stock_Next$open)
        cfadj_Next   <- if(!is.na(stock_Next$cfadj[1])) stock_Next$cfadj[1] else cfadj_T
        adj_ratio <- if(cfadj_T != 0) cfadj_Next / cfadj_T else 1
        if(adj_ratio > 10 || adj_ratio < 0.1) adj_ratio <- 1
        
        S_close_Next_Adj <- S_close_Next * adj_ratio
        S_open_Next_Adj  <- S_open_Next  * adj_ratio
        
        if(!is.na(strike_selected)) {
          payoff_val <- max(0, S_close_Next_Adj - strike_selected)
          status_msg <- ifelse(payoff_val > 0, "被履約 (ITM)", "未履約 (OTM)")
          cost_basis <- S_close_T - premium_received
          # 修正為不含股利
          portfolio_end_val <- S_close_Next_Adj - payoff_val
          if(cost_basis > 0) sbxm_ret <- (portfolio_end_val / cost_basis) - 1
        }
        # 修正為不含股利
        bnh_ret <- (S_close_Next_Adj / S_close_T) - 1
      }
      
      dji_T <- get_dji_price(curr_date, dji_index_df)
      dji_Next <- get_dji_price(next_date, dji_index_df)
      if(!is.na(dji_T) && !is.na(dji_Next) && dji_T > 0) market_ret <- (dji_Next / dji_T) - 1
      
      results_list[[i]] <- data.frame(
        Trading_Date = curr_date, 
        Expiry_Date  = next_date, 
        Stock_Ticker = target_ticker, 
        Moneyness_Setting = moneyness,
        Stock_Price_Close = S_close_T,
        Stock_Price_Open  = S_open_T,     
        Split_Factor      = cfadj_T,
        Call_Strike = strike_selected,
        Premium_Received = premium_received,
        Settlement_Price_close = ifelse(is.na(S_close_Next), NA, S_close_Next),
        Settlement_Payoff = -payoff_val,
        Exercise_Status = status_msg,
        RiskFreeRate_30D = curr_rf,
        
        SBXM_Return = ifelse(is.na(sbxm_ret), 0, round(sbxm_ret, 6)),
        BnH_Return  = ifelse(is.na(bnh_ret), 0, round(bnh_ret, 6)),
        Market_Return = ifelse(is.na(market_ret), 0, round(market_ret, 6))
      )
    }
    
    #5. 輸出結果
    df_final <- do.call(rbind, results_list)
    
    if (!is.null(df_final) && nrow(df_final) > 0) {
      df_final <- df_final %>% 
        filter(!is.na(SBXM_Return))
      
      # [A] 建立XTS (分離計算與繪圖數據)
      ts_ret <- xts(df_final[, c("SBXM_Return", "BnH_Return", "Market_Return")], order.by = df_final$Expiry_Date)
      col_names <- c(paste0("IBXM (M=", moneyness, ")"), 
                     paste0("Buy & Hold (", target_ticker, ")"),
                     "Market (DJI)")
      colnames(ts_ret) <- col_names
      
      ts_rf_monthly <- xts(df_final$RiskFreeRate_30D / 1200, order.by = df_final$Expiry_Date)
      
      # [B] 計算所有績效指標 (使用原始數據)
      tab_ann <- table.AnnualizedReturns(ts_ret[, 1:2], Rf = ts_rf_monthly, scale = 12)
      max_dd  <- maxDrawdown(ts_ret[, 1:2])
      sortino <- SortinoRatio(ts_ret[, 1:2], MAR = 0)
      calmar  <- CalmarRatio(ts_ret[, 1:2], scale = 12)
      treynor_row <- TreynorRatio(Ra = ts_ret[, 1:2], Rb = ts_ret[, 3], Rf = ts_rf_monthly, scale = 12)
      ir_row      <- InformationRatio(Ra = ts_ret[, 1:2], Rb = ts_ret[, 3], scale = 12)
      beta_sbxm  <- CAPM.beta(Ra = ts_ret[, 1], Rb = ts_ret[, 3], Rf = ts_rf_monthly)
      beta_stock <- CAPM.beta(Ra = ts_ret[, 2], Rb = ts_ret[, 3], Rf = ts_rf_monthly)
      beta_row   <- data.frame(SBXM = as.numeric(beta_sbxm), BnH = as.numeric(beta_stock))
      colnames(beta_row) <- colnames(tab_ann)
      
      perf_matrix <- rbind(tab_ann, max_dd, sortino, calmar, 
                           "Beta (vs DJI)" = beta_row, "Treynor Ratio" = treynor_row, "Information Ratio" = ir_row)
      df_perf <- cbind(Metric = rownames(perf_matrix), as.data.frame(perf_matrix))
      
      # [C]. 繪圖 (使用含起始日的數據)
      first_trade_date <- min(df_final$Trading_Date)
      ts_start <- xts(matrix(0, nrow = 1, ncol = 3), order.by = first_trade_date)
      colnames(ts_start) <- col_names
      ts_plot <- rbind(ts_start, ts_ret)
      
      file_tag <- sprintf("M%s", as.character(moneyness * 100))
      temp_plot_file <- paste0(path_output, "/chart_", target_ticker, "_", file_tag, ".png")
      png(filename = temp_plot_file, width = 1200, height = 800, res = 120)
      charts.PerformanceSummary(ts_plot, 
                                main = paste("Strategy vs Asset vs Market:", target_ticker), 
                                wealth.index = TRUE, 
                                colorset = c("blue", "red", "gray"), lwd = 2)
      dev.off()
      
      # [D] 存檔 Excel
      wb <- createWorkbook()
      addWorksheet(wb, "Trade_Log");     writeData(wb, "Trade_Log", df_final)
      addWorksheet(wb, "Performance");   writeData(wb, "Performance", df_perf)
      addWorksheet(wb, "Chart");         insertImage(wb, "Chart", temp_plot_file, width = 10, height = 6, startRow = 2, startCol = 2)
      
      final_filename <- paste0("/IBXM_", target_ticker, ".xlsx")
      saveWorkbook(wb, paste0(path_output, final_filename), overwrite = TRUE)
      if(file.exists(temp_plot_file)) file.remove(temp_plot_file)
      
      success_count <- success_count + 1
      
      
    } else {
      fail_count <- fail_count + 1
      message(paste("⚠️", target_ticker, "無回測結果"))
    }
    
  }, error = function(e) {
    fail_count <<- fail_count + 1
    message(paste("❌ Error processing", target_ticker, ":", e$message))
  })
}

close(pb_total)
dbDisconnect(con)