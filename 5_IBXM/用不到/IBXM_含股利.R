# 1. 套件載入 =====
library(RSQLite)
library(openxlsx)
library(dplyr)
library(lubridate)
library(timeDate)
library(PerformanceAnalytics)
library(xts)
library(readxl) 

# 2. 設定區 =====
current_path <- getwd()  

# 設定路徑
path_db     <- file.path(current_path, "Data", "SQLDB1.sqlite")
path_opt    <- "C:/Users/User/Desktop/Thesis_project/data_raw/DIA_opt" 

# [設定雙軌股價路徑]
path_dia_raw <- "C:/Users/User/Desktop/Thesis_project/data_raw/DIA_還原.xls" # 請確認這份是用於選履約價
path_dia_adj <- "C:/Users/User/Desktop/Thesis_project/data_raw/DIA.xls"      # 用於計算BnH
path_dji_tr  <- "C:/Users/User/Desktop/Thesis_project/data_raw/DJI_還原.xls" # 用於Benchmark

path_output <- file.path(current_path, "Output_IBXM")

# 參數
moneyness   <- 1.00
start_date  <- as.Date("2004-08-31")
end_date    <- as.Date("2023-08-31")
strike_mult <- 1000

# ==============================================================================
# 通用函數：強力讀取 Excel (自動清除 Footer 與 修正日期)
# ==============================================================================
robust_read_excel <- function(path) {
  # 1. 強制全部讀為文字 (避免型別判斷錯誤)
  raw_df <- read_excel(path, col_types = "text", .name_repair = "minimal")
  
  # 2. 選取前兩欄 (假設第1欄是日期, 第2欄是價格)
  df <- raw_df %>% select(1, 2) %>% setNames(c("Date_Str", "Close_Str"))
  
  # 3. 清洗資料：移除含有 "DISCLAIMER", "Source" 或空白的列
  df <- df %>%
    filter(!is.na(Date_Str)) %>%
    filter(!grepl("DISCLAIMER", Date_Str, ignore.case = TRUE)) %>%
    filter(!grepl("Source:", Date_Str, ignore.case = TRUE)) %>%
    filter(!grepl("Dow Jones", Date_Str, ignore.case = TRUE)) # 移除標題行如果重複讀到
  
  # 4. 日期轉換 (處理 Excel Serial 數字字串 vs 標準日期字串)
  df$Date <- suppressWarnings(
    ifelse(grepl("^[0-9]+$", df$Date_Str), 
           # 如果是純數字 (如 43511) -> 轉為 Excel Serial Date
           as.Date(as.numeric(df$Date_Str), origin = "1899-12-30"),
           # 否則嘗試直接轉 Date
           as.Date(df$Date_Str)
    )
  )
  # 轉回來確保是 Date 物件 (ifelse 會把 Date 轉成數值)
  df$Date <- as.Date(df$Date, origin = "1970-01-01")
  
  # 5. 價格轉換
  df$Close <- suppressWarnings(as.numeric(df$Close_Str))
  
  # 6. 過濾無效資料
  df_final <- df %>% 
    filter(!is.na(Date), !is.na(Close)) %>% 
    select(Date, Close) %>% 
    arrange(Date)
  
  return(df_final)
}

# 3. 讀取資料 =====
message("正在連線SQL資料庫...")
con <- dbConnect(RSQLite::SQLite(), path_db)

# [A] 設定回測標的
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

# [C] 大盤指數 (使用強力讀取函數)
# -----------------------------------------------------------
if(file.exists(path_dji_tr)) {
  message("正在讀取 DJI Total Return (基準)...")
  dji_index_df <- robust_read_excel(path_dji_tr)
  message(paste("✅ DJI TR 讀取成功，共", nrow(dji_index_df), "筆數據"))
} else {
  message("⚠️ 找不到 DJI_還原.xls，退回使用 SQL dji_index (Price Return)")
  dji_temp <- dbReadTable(con, "dji_index")
  names(dji_temp) <- tolower(names(dji_temp))
  dji_index_df <- dji_temp %>%
    mutate(Date = as.Date(date), Close = as.numeric(close)) %>%
    select(Date, Close) %>% arrange(Date)
}
# -----------------------------------------------------------

get_dji_price <- function(target_date, dji_df) {  
  subset <- dji_df %>% filter(Date <= target_date)
  if (nrow(subset) == 0) return(NA)
  return(tail(subset, 1)$Close)
}

# [D] 產生交易日曆
month_seq <- seq(floor_date(start_date, "month"), floor_date(end_date, "month"), by = "month")
get_3rd_friday <- function(date_obj) {
  # 產生當月所有日期
  days <- seq(floor_date(date_obj, "month"), ceiling_date(date_obj, "month") - days(1), by = "day")
  
  fridays <- days[lubridate::wday(days) == 6] 
  
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
    # [A] 設定有效期間
    schedule <- schedule_global
    if(nrow(schedule) == 0) stop("無有效期間")
    
    # [B] 讀取並合併兩份股價資料 (使用強力讀取函數)
    if (!file.exists(path_dia_raw)) stop(paste("找不到原始股價檔案:", path_dia_raw))
    if (!file.exists(path_dia_adj)) stop(paste("找不到還原股價檔案:", path_dia_adj))
    
    df_raw <- robust_read_excel(path_dia_raw) %>% rename(close_raw = Close)
    df_adj <- robust_read_excel(path_dia_adj) %>% rename(close_adj = Close)
    
    # 合併為單一 stock_df
    stock_df <- inner_join(df_raw, df_adj, by = "Date") %>%
      rename(date = Date) %>% # 轉小寫配合後面程式碼
      mutate(
        open = close_raw, 
        cfadj = 1
      ) %>%
      arrange(date)
    
    if(nrow(stock_df) == 0) stop("股價資料合併後為空")
    
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
      
      # *** 區分 Raw 與 Adj ***
      S_close_T_Raw <- as.numeric(stock_T$close_raw)
      S_close_T_Adj <- as.numeric(stock_T$close_adj)
      S_open_T      <- as.numeric(stock_T$open)
      cfadj_T       <- 1
      curr_rf       <- get_target_rf(curr_date, rf_raw)
      
      # 讀取RDS
      file_tag <- format(curr_date, "%Y%m")
      rds_files <- list.files(path_opt, pattern = paste0(file_tag, ".*DIA.*\\.rds$"), full.names = TRUE, ignore.case = TRUE)
      
      opt_raw <- NULL; premium_received <- 0; strike_selected <- NA
      
      if(length(rds_files) > 0) {
        tryCatch({
          opt_raw <- readRDS(rds_files[1]) 
          if(!is.null(opt_raw$temp_date)) {
            valid_dates <- unique(opt_raw$temp_date[opt_raw$temp_date <= curr_date & opt_raw$temp_date >= (curr_date - 5)])
            if(length(valid_dates) > 0) {
              use_date <- max(valid_dates)
              target_opts <- opt_raw %>% 
                filter(temp_date == use_date, as.Date(exdate) == next_date, cp_flag == "C") %>% 
                mutate(real_strike = strike_price / strike_mult)
              
              if ("contract_size" %in% names(opt_raw) && any(target_opts$contract_size == 100)) {
                target_opts <- target_opts %>% filter(contract_size == 100)
              }
              
              target_strike_price <- S_close_T_Raw * moneyness
              candidates <- target_opts %>% filter(real_strike >= S_close_T_Raw) 
              
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
      stock_Next <- stock_df %>% filter(date == next_date)
      if(nrow(stock_Next)==0) stock_Next <- stock_df %>%
        filter(date >= next_date-1 & date <= next_date+2) %>%
        head(1)
      
      sbxm_ret <- NA; bnh_ret <- NA; market_ret <- NA
      status_msg <- "資料不足"; payoff_val <- 0; S_open_Next <- NA
      S_close_Next_Raw <- NA
      
      if(nrow(stock_Next) > 0) {
        S_close_Next_Raw <- as.numeric(stock_Next$close_raw) 
        S_close_Next_Adj <- as.numeric(stock_Next$close_adj) 
        
        # 選擇權結算 (看原始股價)
        if(!is.na(strike_selected)) {
          payoff_val <- max(0, S_close_Next_Raw - strike_selected)
          status_msg <- ifelse(payoff_val > 0, "被履約 (ITM)", "未履約 (OTM)")
          
          # 策略回報 (使用 Adj)
          cost_basis <- S_close_T_Adj - premium_received
          portfolio_end_val <- S_close_Next_Adj - payoff_val
          
          if(cost_basis > 0) sbxm_ret <- (portfolio_end_val / cost_basis) - 1
        }
        
        # BnH 回報 (使用 Adj)
        bnh_ret <- (S_close_Next_Adj / S_close_T_Adj) - 1
      }
      
      dji_T <- get_dji_price(curr_date, dji_index_df)
      dji_Next <- get_dji_price(next_date, dji_index_df)
      if(!is.na(dji_T) && !is.na(dji_Next) && dji_T > 0) market_ret <- (dji_Next / dji_T) - 1
      
      results_list[[i]] <- data.frame(
        Trading_Date = curr_date, 
        Expiry_Date  = next_date, 
        Stock_Ticker = target_ticker, 
        Moneyness_Setting = moneyness,
        Stock_Price_Close = S_close_T_Raw,
        Stock_Price_Open  = S_open_T,     
        Split_Factor      = cfadj_T,
        Call_Strike = strike_selected,
        Premium_Received = premium_received,
        Settlement_Price_close = ifelse(is.na(S_close_Next_Raw), NA, S_close_Next_Raw),
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
      
      # [A] 建立XTS
      ts_ret <- xts(df_final[, c("SBXM_Return", "BnH_Return", "Market_Return")], order.by = df_final$Expiry_Date)
      col_names <- c(paste0("IBXM (M=", moneyness, ")"), 
                     paste0("Buy & Hold (", target_ticker, ")"),
                     "Benchmark (DJI TR)") # 這裡改名以避免混淆
      colnames(ts_ret) <- col_names
      
      ts_rf_monthly <- xts(df_final$RiskFreeRate_30D / 1200, order.by = df_final$Expiry_Date)
      
      # [B] 計算績效指標
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
                           "Beta (vs DJI TR)" = beta_row, 
                           "Treynor Ratio" = treynor_row, 
                           "Information Ratio" = ir_row)
      df_perf <- cbind(Metric = rownames(perf_matrix), as.data.frame(perf_matrix))
      
      # [C]. 繪圖
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