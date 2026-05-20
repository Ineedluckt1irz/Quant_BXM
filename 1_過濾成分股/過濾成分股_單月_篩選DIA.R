# ------------------------------------------------------------
# Single Month Filter for DIA and DJX
# 專門針對單一月份 (例如 201809) 的快速處理版本
# ------------------------------------------------------------
suppressPackageStartupMessages({
  library(data.table); library(readxl); library(stringr); library(tools)
})

# ========= 1. 設定區 =========
target_month <- "201810"
opt_dir      <- "C:/Users/User/Desktop/Thesis_project/DIA"
out_dir      <- "C:/Users/User/Desktop/Thesis_project/data_raw/DIA_opt"
target_tickers <- c("DIA", "DJX")

# 確保輸出目錄存在
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ========= 2. 定位檔案 =========
# 直接尋找檔名開頭為 201809 的檔案
target_file <- list.files(opt_dir, pattern = paste0("^", target_month), full.names = TRUE)[1]

if (is.na(target_file)) {
  stop("找不到目標檔案：", target_month)
}

message("正在處理檔案：", basename(target_file))

# ========= 3. 讀取與過濾 (串流處理版) =========

# 1. 取得標頭 (Header)
con <- file(target_file, "r")
header_line <- readLines(con, n = 1)
header <- strsplit(header_line, ",")[[1]] # 假設是逗號分隔

# 2. 建立暫存清單來儲存匹配的行
filtered_lines <- list()
filtered_lines[[1]] <- header_line

message("開始掃描檔案內容 (這可能需要幾分鐘)...")

# 3. 逐行掃描 (一次讀 10 萬行，避免記憶體壓力)
while (TRUE) {
  lines <- readLines(con, n = 100000)
  if (length(lines) == 0) break
  
  # 找出含有目標代碼的行 (不區分大小寫)
  matches <- grep("DIA|DJX", lines, ignore.case = TRUE, value = TRUE)
  if (length(matches) > 0) {
    filtered_lines <- c(filtered_lines, matches)
  }
}
close(con)

# 4. 將過濾後的文字轉成 data.table
if (length(filtered_lines) > 1) {
  dt_keep <- fread(text = paste(filtered_lines, collapse = "\n"))
  
  # 5. 二次清洗 (確保 Ticker 準確)
  tick_col <- grep("ticker", names(dt_keep), ignore.case = TRUE, value = TRUE)[1]
  dt_keep[, ticker_key := toupper(gsub("\\s+", "", sub("^.*?:", "", as.character(get(tick_col)))))]
  dt_keep <- dt_keep[ticker_key %in% target_tickers]
} else {
  stop("在檔案中找不到任何 DIA 或 DJX 的數據。")
}# ========= 4. 存檔與報告 =========
out_path <- file.path(out_dir, paste0(target_month, "_DIA.csv"))
fwrite(dt_keep, out_path)

cat("\n==============================\n")
cat("處理完成！\n")
cat("原始列數：", nrow(dt), "\n")
cat("保留列數：", nrow(dt_keep), "\n")
cat("包含代碼：", paste(unique(dt_keep$ticker_key), collapse = ", "), "\n")
cat("檔案路徑：", out_path, "\n")