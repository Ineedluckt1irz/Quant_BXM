library(DBI)
library(RSQLite)
library(readxl)
library(dplyr)

# 1. 設定路徑
db_dir <- "C:/Users/User/Desktop/Thesis_project/Data/"
db_path <- paste0(db_dir, "SQLDB1.sqlite")
base_dir <- "C:/Users/User/Desktop/Thesis_project/data_raw/"

if (!dir.exists(db_dir)) dir.create(db_dir, recursive = TRUE)

con <- dbConnect(RSQLite::SQLite(), db_path)
message("✅ 資料庫連線成功")

# 強化版日期修正函數：自動處理 Excel 數字日期與字串日期
fix_date_column <- function(df, standard_name = "date") {
  target <- grep("date", colnames(df), ignore.case = TRUE, value = TRUE)[1]
  if (!is.na(target)) {
    colnames(df)[colnames(df) == target] <- standard_name
    # 判斷是否為 Excel 數值型日期
    if (is.numeric(df[[standard_name]])) {
      df[[standard_name]] <- as.character(as.Date(df[[standard_name]], origin = "1899-12-30"))
    } else {
      df[[standard_name]] <- as.character(as.Date(df[[standard_name]]))
    }
  }
  return(df)
}

# ==============================================================================
# 檔案 1: 無風險利率 (CSV)
# ==============================================================================
path_rf <- paste0(base_dir, "Zero Coupon Yield Curve(~20230831).csv")
if (file.exists(path_rf)) {
  df_rf <- read.csv(path_rf) %>% fix_date_column("date") %>%
    mutate(days = as.numeric(days), rate = as.numeric(rate))
  
  dbWriteTable(con, "rf_rate", df_rf, overwrite = TRUE, row.names = FALSE)
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_rf_date ON rf_rate (date)")
  message("✅ rf_rate 寫入完成")
}

# ==============================================================================
# 檔案 2: 大盤總報酬指數 (Excel) -> 修正更名問題
# ==============================================================================
path_djitr <- paste0(base_dir, "DJITR_19960101-20260127.xls")
if (file.exists(path_djitr)) {
  df_djitr <- read_excel(path_djitr)
  # 重新命名欄位為回測所需的 Date, Close
  colnames(df_djitr)[1:2] <- c("Date", "Close")
  df_djitr <- df_djitr %>% fix_date_column("Date") %>%
    mutate(Close = as.numeric(Close))
  
  dbWriteTable(con, "dji_index", df_djitr, overwrite = TRUE, row.names = FALSE)
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_dji_date ON dji_index (Date)")
  message("✅ dji_index 寫入完成")
}

# ==============================================================================
# 檔案 3: CRSP 股價資料 (CSV) -> 關鍵更名處：open, close, cfadj
# ==============================================================================
path_prices <- paste0(base_dir, "CRSP股價/DJI_Prices.csv")
if (file.exists(path_prices)) {
  message("🚀 正在處理大型股價檔案 (更名中)...")
  df_prices <- read.csv(path_prices, stringsAsFactors = FALSE) %>% 
    fix_date_column("date") %>%
    # 修正欄位名稱以符合回測程式碼
    rename(
      open  = openprc,
      close = prc,
      cfadj = cfacpr
    ) %>%
    # 確保數值欄位是數字
    mutate(
      open  = as.numeric(open),
      close = as.numeric(close),
      cfadj = as.numeric(cfadj)
    )
  
  dbWriteTable(con, "stock_prices", df_prices, overwrite = TRUE, row.names = FALSE)
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_stock_ticker_date ON stock_prices (ticker, date)")
  message("✅ stock_prices 寫入完成 (欄位已更名為 open, close, cfadj)")
  rm(df_prices); gc()
}

# ==============================================================================
# 檔案 4: DJI 成分股名單 (Excel)
# ==============================================================================
path_const <- paste0(base_dir, "DJI成分股名單.xls")
if (file.exists(path_const)) {
  df_const <- read_excel(path_const)
  dbWriteTable(con, "dji_constituents_raw", as.data.frame(df_const), overwrite = TRUE, row.names = FALSE)
  message("✅ dji_constituents_raw 寫入完成")
}

# ==============================================================================
# 檔案 5: 股利發放紀錄 (CSV)
# ==============================================================================
path_div <- paste0(base_dir, "Dividend Distribution History(19960104~20230831).csv")
if (file.exists(path_div)) {
  df_div <- read.csv(path_div) %>%
    mutate(ex_date = as.character(as.Date(ex_date)),
           amount = as.numeric(amount))
  
  dbWriteTable(con, "dividend_history", df_div, overwrite = TRUE, row.names = FALSE)
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_div_exdate ON dividend_history (ex_date)")
  message("✅ dividend_history 寫入完成")
}

# ==============================================================================
# 檔案6: DIA ETF 還原指數 (Excel)
# ==============================================================================
path_dia <- paste0(base_dir, "DIA_還原.xls")
if (file.exists(path_dia)) {
  df_dia <- read_excel(path_dia)
  
  # 假設前兩欄為 日期 / 還原指數
  colnames(df_dia)[1:2] <- c("Date", "Close")
  
  df_dia <- df_dia %>%
    fix_date_column("Date") %>%
    mutate(Close = as.numeric(Close))
  
  dbWriteTable(con, "dia_index", df_dia, overwrite = TRUE, row.names = FALSE)
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_dia_date ON dia_index (Date)")
  message("✅ dia_index 寫入完成")
}


# 結尾
print(dbListTables(con))
dbDisconnect(con)
message("👋 任務結束，資料庫已更新。")