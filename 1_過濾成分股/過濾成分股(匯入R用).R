# ============================================================
# 純過濾模式 (不需 tidyr 版)
# 改用 data.table::melt 取代 pivot_longer，解決安裝問題
# ============================================================

# 1. 載入套件 (不需要 tidyr 了)
library(data.table) # 核心運算
library(readxl)     # 讀 Excel
library(dplyr)      # 資料處理輔助
library(stringr)    # 字串處理
library(lubridate)  # 日期處理

# 2. 檢查 opt 變數是否存在
# 【請修改】將下方的 X202005 改為你 R 裡面實際的變數名稱
if (exists("X202005")) {
  opt <- X202005 
} else {
  if (!exists("opt")) stop("找不到變數 'opt'！請確認你已經讀入資料。")
}
setDT(opt) # 轉為 data.table 提升效能

# 3. 設定路徑與月份
constituents_xlsx <- "C:/Users/User/Desktop/OptionMetrics/DJI成分股名單.xlsx"
target_month_str  <- "202005"  # 目標月份

# 4. 定義清洗函數
nuclear_clean <- function(v) {
  v <- as.character(v)
  v <- sub("^.*?:", "", v)
  v <- toupper(v)
  v <- gsub("[^A-Z0-9]", "", v)
  return(v)
}

# 5. 抓取成分股名單 (改用 melt)
message("--> 正在分析成分股名單...")
con_raw <- as.data.table(read_excel(constituents_xlsx)) # 讀進來直接轉 data.table

# 抓取欄位
date_cols <- names(con_raw)[str_detect(names(con_raw), "^[A-Za-z]{3}-\\d{2}-\\d{4} % Of CSO$")]
tick_col  <- intersect(c("Ticker","ticker","Symbol","Exchange:Ticker"), names(con_raw))[1]

# 【關鍵修改】使用 melt 取代 pivot_longer
con_long <- melt(con_raw, 
                 measure.vars = date_cols, 
                 variable.name = "as_of", 
                 value.name = "pct")

# 處理日期與篩選成分股
con_long[, as_of := as.character(as_of)] # 確保是字串
con_long[, snap := mdy(str_remove(as_of, "\\s+% Of CSO$"), locale = "C")]
con_long[, pct_num := suppressWarnings(as.numeric(pct))]
con_long <- con_long[!is.na(pct_num) & pct_num > 0] # 只留成分股

# 處理 Ticker 清洗
# 注意：這裡使用 data.table 的語法進行 merge 與清洗
con_long[, ticker_raw := get(tick_col)] 
con_long[, ticker_clean := nuclear_clean(ticker_raw)]

# 6. 鎖定日期與名單
target_date <- ymd(paste0(target_month_str, "01"))
snap_cutoff <- max(con_long[snap < target_date, snap]) 
snap_tickers <- unique(con_long[snap == snap_cutoff, ticker_clean])

message("--> 使用快照日：", as.character(snap_cutoff))
message("--> 應有成分股：", length(snap_tickers), " 檔")

# 7. 開始過濾 Options 資料
message("--> 正在過濾資料...")
names(opt) <- tolower(gsub("[^A-Za-z0-9]", "", names(opt))) # 清洗欄位名

if (!"ticker" %in% names(opt)) {
  if ("symbol" %in% names(opt)) opt[, ticker := symbol] else stop("找不到 ticker 欄位！")
}

opt[, ticker_key := nuclear_clean(ticker)] # 建立比對鍵
opt_keep <- opt[ticker_key %in% snap_tickers] # 過濾

opt[, ticker_key := NULL]      # 清理暫存
opt_keep[, ticker_key := NULL]

# 8. 輸出結果
cat("\n[結果報告]\n")
cat("原始筆數：", format(nrow(opt), big.mark=","), "\n")
cat("保留筆數：", format(nrow(opt_keep), big.mark=","), "\n")

if (nrow(opt_keep) > 0) {
  out_path <- file.path(dirname(constituents_xlsx), paste0("成分股_", target_month_str, ".csv"))
  fwrite(opt_keep, out_path)
  cat("✅ 成功！檔案已存到：", out_path, "\n")
} else {
  cat("⚠️ 警告：過濾後變為 0 筆。\n")
}