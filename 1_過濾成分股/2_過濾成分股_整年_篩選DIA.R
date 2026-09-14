suppressPackageStartupMessages({
  library(data.table); library(readxl); library(dplyr); library(lubridate); library(tools)
})

# ========= 設定 =========
opt_dir      <- "C:/Users/User/Desktop/Thesis_project/DIA"
out_dir      <- "C:/Users/User/Desktop/Thesis_project/data_raw/DIA_opt"
year_target  <- 2013
target_tickers <- c("DIA", "DJX")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ========= 1. 建立月份與檔案的映射表 (避免重複讀取) =========
months_vec <- sprintf("%d%02d", year_target, 1:12)
file_map <- data.table(target_month = months_vec, file_path = as.character(NA))

all_files <- list.files(opt_dir, pattern = "\\.(csv|tsv|txt|xlsx|xls)$", full.names = TRUE, ignore.case = TRUE)
file_names <- basename(all_files)

for (i in seq_along(months_vec)) {
  m <- months_vec[i]
  curr_m_num <- as.numeric(substr(m, 5, 6))
  
  for (offset in 0:5) {
    check_m_num <- curr_m_num + offset
    if (check_m_num > 12) break
    
    pattern <- paste0("^", sprintf("%d%02d", year_target, check_m_num))
    match_idx <- grep(pattern, file_names)
    
    if (length(match_idx) > 0) {
      file_map[i, file_path := all_files[match_idx[1]]]
      break
    }
  }
}

# 只處理有找到檔案的
file_to_process <- file_map[!is.na(file_path), unique(file_path)]

# ========= 2. 核心處理函數 (優化讀取與過濾) =========
process_bundle <- function(path, target_months) {
  cat("\n讀取檔案：", basename(path), "\n")
  
  # 使用 fread 的選擇性讀取 (select) 可以更快，但若格式不固定則維持 read_any
  ext <- tolower(file_ext(path))
  dt <- if (ext %in% c("csv","tsv","txt")) {
    fread(path, sep = "auto", nThread = 4) # 開啟多線程
  } else {
    as.data.table(read_excel(path))
  }
  
  # 欄位名稱標準化 (一次性處理)
  names(dt) <- toupper(names(dt))
  date_col <- grep("DATE", names(dt), value = TRUE)
  date_col <- date_col[!grepl("EX", date_col)][1]
  tick_col <- grep("TICKER", names(dt), value = TRUE)[1]
  
  if (is.na(date_col) || is.na(tick_col)) return(NULL)
  
  # 預處理：轉日期與清洗 Ticker (向量化運算極快)
  dt[, DATE_DT := ymd(get(date_col), quiet = TRUE)]
  dt[, YM := format(DATE_DT, "%Y%m")]
  dt[, TICKER_CLEAN := gsub("\\s+", "", sub("^.*?:", "", as.character(get(tick_col))))]
  
  # 只保留目標 Tickers
  dt <- dt[TICKER_CLEAN %in% target_tickers]
  
  # 依照此檔案含有的月份進行切分存檔
  available_months <- intersect(unique(dt$YM), target_months)
  
  for (m in available_months) {
    out_path <- file.path(out_dir, paste0(m, "_DIA.csv"))
    fwrite(dt[YM == m], out_path)
    cat("  -> 已存檔：", m, "[", nrow(dt[YM == m]), "rows ]\n")
  }
  rm(dt); gc() # 強制釋放記憶體
}

# ========= 3. 執行批次 (按檔案分組) =========
message("開始加速處理...")
walk(file_to_process, ~{
  # 找出這個檔案負責哪些月份
  needed_months <- file_map[file_path == .x, target_month]
  process_bundle(.x, needed_months)
})

message("\n處理完成！")