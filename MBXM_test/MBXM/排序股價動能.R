# ==============================================================================
# 模組：MBXM 動能排名生成器 (對齊完美標準版 + 參數化穩健性測試)
# ==============================================================================

# 1. 套件載入 =====
library(tidyverse)
library(lubridate)
library(openxlsx)
library(timeDate)
library(data.table)
library(RSQLite)

# 2. 設定區 (參數調整區) =====
path_db        <- "C:/Users/User/Desktop/Thesis_project/Data/SQLDB1.sqlite"
path_output    <- "C:/Users/User/Desktop/Thesis_project/Output_MBXM"
path_csv_price <- "C:/Users/User/Desktop/Thesis_project/data_raw/CRSP股價/DJI_Prices.csv"

# --- [穩健性測試參數區] ---
momentum_window <- 90   # 天數 (90, 180, 360)
top_n_threshold <- 10   # 強勢股數設定
# -------------------------

if(!dir.exists(path_output)) dir.create(path_output, recursive = TRUE)

user_start_date <- "2004-08-31"
user_end_date   <- "2023-08-31"

# [特殊標的 CUSIP 映射表] 依照完美標準版
special_cusip_map <- list(
  "DD(OLD)" = "26353410", "AA" = "44320110", 
  "MTLQ" = c("62010A10", "37044210"), "EK" = "27746110", 
  "T(OLD)"  = "00195750", "DD" = "26614N10", "RTX" = "75513E10"
)

# 3. 資料讀取與預處理 =====
message("🔍 正在讀取 SQL 成分股歷史資料...")
con <- dbConnect(RSQLite::SQLite(), path_db)
constituents_raw <- dbReadTable(con, "dji_constituents_raw")

# 欄位與日期解析
date_cols <- names(constituents_raw)[grepl("Equity", names(constituents_raw), ignore.case = TRUE)]
date_cols <- date_cols[!grepl("Latest", date_cols, ignore.case = TRUE)]
ticker_col <- names(constituents_raw)[tolower(names(constituents_raw)) == "ticker"][1]

constituents_long <- constituents_raw %>%
  select(all_of(c(ticker_col, date_cols))) %>%
  pivot_longer(cols = -all_of(ticker_col), names_to = "Date_Str", values_to = "Weight") %>%
  filter(!is.na(Weight) & Weight > 0) %>%
  mutate(
    Clean_Date = str_replace_all(Date_Str, "[^a-zA-Z0-9]", " "),
    Clean_Date = str_remove_all(Clean_Date, "(?i)of equity"),
    Comp_Date_Obj = as.Date(parse_date_time(Clean_Date, orders = c("mdy", "bdy", "dmy", "ymd"))),
    Ticker = toupper(!!sym(ticker_col))
  ) %>%
  filter(!is.na(Comp_Date_Obj)) %>%
  select(Comp_Date_Obj, Ticker)

message("🔍 正在讀取股價資料並套用 CUSIP 映射...")
all_prices <- fread(path_csv_price) %>%
  mutate(
    date = as.Date(date), ticker = toupper(ticker), close = abs(as.numeric(prc)),
    cusip_str = str_pad(as.character(cusip), 8, side = "left", pad = "0")
  ) %>%
  mutate(ticker = case_when(
    cusip_str == "26353410" ~ "DD(OLD)", cusip_str == "44320110" ~ "AA",
    cusip_str %in% c("62010A10", "37044210") ~ "MTLQ", cusip_str == "27746110" ~ "EK",
    cusip_str == "00195750" ~ "T(OLD)", cusip_str == "26614N10" ~ "DD",
    cusip_str == "75513E10" ~ "RTX", TRUE ~ ticker
  )) %>%
  filter(!is.na(date), !is.na(close)) %>%
  select(date, ticker, close) %>%
  distinct(date, ticker, .keep_all = TRUE)

dbDisconnect(con)

# 4. 產生交易日曆 =====
month_seq <- seq(floor_date(as.Date(user_start_date), "month"), floor_date(as.Date(user_end_date), "month"), by = "month")
trade_dates <- as.Date(sapply(month_seq, function(d) {
  days <- seq(floor_date(d, "month"), ceiling_date(d, "month") - days(1), by = "day")
  fridays <- days[wday(days) == 6]
  target <- if(length(fridays) >= 3) fridays[3] else NA
  if(is.na(target)) return(NA)
  nyse_hols <- as.Date(holidayNYSE(year(target)))
  if(target %in% nyse_hols) target <- target - days(1)
  return(target)
}), origin = "1970-01-01")
trade_dates <- trade_dates[!is.na(trade_dates)]

# 5. 核心動能計算迴圈 =====
message(paste0("🚀 開始計算 ", momentum_window, " 天動能排名..."))
rank_results <- list()
pb <- txtProgressBar(min = 0, max = length(trade_dates), style = 3)
comp_report_dates <- sort(unique(constituents_long$Comp_Date_Obj))

for (i in seq_along(trade_dates)) {
  curr_trade_date <- trade_dates[i]
  setTxtProgressBar(pb, i)
  
  target_report_date <- max(comp_report_dates[comp_report_dates <= curr_trade_date], na.rm = TRUE)
  current_30_tickers <- constituents_long %>% filter(Comp_Date_Obj == target_report_date) %>% pull(Ticker)
  
  obs_end_date   <- curr_trade_date - days(1)
  obs_start_date <- obs_end_date - days(momentum_window)
  
  momentum_data <- all_prices %>%
    filter(ticker %in% current_30_tickers) %>% 
    filter(date <= obs_end_date & date >= obs_start_date - 14) %>% 
    group_by(ticker) %>%
    summarise(Price_End = tail(close, 1), Price_Start = head(close, 1), .groups = 'drop') %>%
    mutate(
      Return_Mom = (Price_End - Price_Start) / Price_Start,
      Trade_Date = curr_trade_date, Momentum_Start = obs_start_date, Momentum_End = obs_end_date
    ) %>%
    filter(!is.finite(Return_Mom) == FALSE) %>%
    arrange(desc(Return_Mom)) %>%
    mutate(Rank = row_number())
  
  rank_results[[i]] <- momentum_data
}
close(pb)

# 6. 標記與輸出 (使用參數化門檻) =====
if (length(rank_results) > 0) {
  final_ranking_df <- bind_rows(rank_results) %>%
    group_by(Trade_Date) %>%
    mutate(
      # 使用參數設定的 top_n_threshold 進行判定 [cite: 50, 178]
      Is_Strong_Stock = if_else(Rank <= top_n_threshold, 1, 0)
    ) %>%
    ungroup() %>%
    select(Trade_Date, Momentum_Start, Momentum_End, Ticker = ticker, 
           Price_Start, Price_End, Return_Mom, Rank, Is_Strong_Stock)
  
  # 檔名加入參數標記，方便區分不同實驗結果 [cite: 51]
  out_file <- file.path(path_output, paste0("MBXM_Rank_", momentum_window, "d_Top", top_n_threshold, ".xlsx"))
  write.xlsx(final_ranking_df, out_file)
  
  message(paste("\n✅ 處理完成！"))
  message(paste("📊 動能回顧天數:", momentum_window, "天"))
  message(paste("📊 強勢股判定門檻: 前", top_n_threshold, "名"))
  message(paste("📂 檔案存檔路徑:", out_file))
}