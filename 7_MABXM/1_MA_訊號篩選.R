# ==============================================================================
# 模組：MABXM 趨勢訊號生成器 (對齊教授指導：20MA vs 180MA 趨勢濾網)
# ==============================================================================

# 1. 套件載入 =====
library(tidyverse)
library(lubridate)
library(openxlsx)
library(timeDate)
library(data.table)
library(zoo)

# 2. 設定區 (請依照你的環境路徑修改) =====
path_csv_price <- "C:/Users/User/Desktop/Thesis_project/data_raw/CRSP股價/DJI_Prices.csv"
path_output    <- "C:/Users/User/Desktop/Thesis_project/Output_MABXM"

if(!dir.exists(path_output)) dir.create(path_output, recursive = TRUE)

user_start_date <- "2004-01-01" # 建議比正式回測早半年，以利 180MA 計算
user_end_date   <- "2023-08-31"

# 3. 讀取股價資料 =====
message("🔍 正在讀取 DJI 股價資料進行 MA 計算...")
all_prices <- fread(path_csv_price) %>%
  mutate(
    date = as.Date(date),
    ticker = toupper(ticker),
    close = abs(as.numeric(prc))
  ) %>%
  filter(!is.na(date), !is.na(close)) %>%
  select(date, ticker, close) %>%
  arrange(ticker, date)

# 4. 計算每日 MA 數值 (使用交易日 Rolling) =====
message("📈 計算 20MA 與 180MA (以交易日為準)...")
ma_data <- all_prices %>%
  group_by(ticker) %>%
  mutate(
    MA20  = rollmean(close, k = 20,  fill = NA, align = "right"),
    MA180 = rollmean(close, k = 180, fill = NA, align = "right")
  ) %>%
  ungroup() %>%
  filter(!is.na(MA180)) # 排除初期資料不足的部分

# 5. 產生「訊號觀察日」清單 (每個月第三個星期四) =====
message("📅 正在計算每個月第三個星期四 (訊號觀察日)...")
month_seq <- seq(floor_date(as.Date(user_start_date), "month"), 
                 floor_date(as.Date(user_end_date), "month"), by = "month")

# 計算第三個星期五 (交易日) 與其前一天 (觀察日)
date_map <- map_df(month_seq, function(d) {
  days <- seq(floor_date(d, "month"), ceiling_date(d, "month") - days(1), by = "day")
  fridays <- days[wday(days) == 6] # 6 為星期五
  
  if(length(fridays) >= 3) {
    third_friday <- fridays[3]
    # 觀察日為前一個交易日 (通常是星期四)
    # 若星期五遇國定假日，交易會提前，但教授邏輯通常指「標準第三個星期四收盤」
    obs_thursday <- third_friday - days(1) 
    
    return(data.frame(Trade_Date = third_friday, Obs_Date = obs_thursday))
  } else {
    return(NULL)
  }
})

# 6. 匹配訊號並判定規則 =====
message("⚖️ 執行趨勢判定規則: 20MA > 180MA 則不賣買權...")
final_ma_signals <- date_map %>%
  inner_join(ma_data, by = c("Obs_Date" = "date")) %>%
  mutate(
    # [cite_start]教授邏輯：20MA > 180MA 代表強勢多頭 [cite: 31]
    Is_Bull_Trend = if_else(MA20 > MA180, 1, 0),
    # 決策：多頭不賣 (No_Write = 1)，非多頭才執行 Buy-Write
    Action_No_Write = if_else(Is_Bull_Trend == 1, 1, 0) 
  ) %>%
  select(Trade_Date, Obs_Date, Ticker = ticker, Close = close, MA20, MA180, Is_Bull_Trend, Action_No_Write)

# 7. 輸出結果 =====
out_file <- file.path(path_output, "MABXM_Signals_DailyMA.xlsx")
write.xlsx(final_ma_signals, out_file)

message(paste("\n✅ 處理完成！"))
message(paste("📊 總計產生訊號筆數:", nrow(final_ma_signals)))
message(paste("📂 檔案存檔路徑:", out_file))