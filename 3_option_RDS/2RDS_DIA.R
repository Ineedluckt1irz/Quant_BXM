# ==============================================================================
# 檔案名稱：03_Data_Conversion_to_RDS.R
# 功能：將原始 OptionMetrics Excel/CSV 轉換為極速 RDS 格式
# ==============================================================================

library(dplyr)
library(readxl)
library(lubridate)
library(tools)

# 1. 設定路徑 =====
# 原始檔案資料夾 (你現在放 CSV/Excel 的地方)
path_source <- "C:/Users/User/Desktop/Thesis_project/data_raw/DIA_opt"

# 目標資料夾 (要存放 RDS 的地方)
path_target <- "C:/Users/User/Desktop/Thesis_project/data_raw/DIA_opt"

# 建立目標資料夾
if (!dir.exists(path_target)) dir.create(path_target, recursive = TRUE)

# 2. 取得所有檔案列表 =====
files <- list.files(path_source, full.names = TRUE)
# 過濾出 .csv 或 .xls/xlsx 檔案
files <- files[grepl("\\.(csv|xls|xlsx)$", files, ignore.case = TRUE)]

message(paste("📂 找到", length(files), "個檔案，準備開始轉檔..."))

# 3. 開始轉檔迴圈 =====
# 建立進度條
pb <- txtProgressBar(min = 0, max = length(files), style = 3)

for (i in seq_along(files)) {
  file_path <- files[i]
  file_name <- basename(file_path)
  file_ext  <- file_ext(file_name)
  
  # 設定輸出的 RDS 檔名 (保持原本的主檔名，改副檔名為 .rds)
  rds_name <- sub(paste0("\\.", file_ext, "$"), ".rds", file_name, ignore.case = TRUE)
  rds_path <- file.path(path_target, rds_name)
  
  # 如果目標檔案已經存在，就跳過 (避免重複轉檔浪費時間)
  if (file.exists(rds_path)) {
    setTxtProgressBar(pb, i)
    next
  }
  
  tryCatch({
    # A. 讀取檔案
    df <- NULL
    if (grepl("csv", file_ext, ignore.case = TRUE)) {
      df <- read.csv(file_path)
    } else {
      df <- read_excel(file_path)
    }
    
    # B. 基本清洗 (統一欄位名稱與日期格式，這樣回測時就不用再洗一次)
    if (!is.null(df)) {
      names(df) <- tolower(names(df)) # 轉小寫
      
      # 處理日期欄位 (Date)
      date_col <- names(df)[grepl("date", names(df)) & !grepl("ex", names(df))]
      if(length(date_col) > 0) {
        # 嘗試標準化日期
        raw_date <- df[[date_col[1]]]
        # 如果是 Excel 數字 (例如 43466)
        if (is.numeric(raw_date)) {
          df$temp_date <- as.Date(raw_date, origin = "1899-12-30")
        } else {
          # 如果是字串或數字 (例如 20190101)
          df$temp_date <- suppressWarnings(ymd(raw_date))
          # 如果 ymd 失敗，嘗試 as.Date
          if (any(is.na(df$temp_date))) {
            df$temp_date <- as.Date(as.character(raw_date), format="%Y-%m-%d")
          }
        }
      }
      
      # 處理到期日 (Exdate)
      if ("exdate" %in% names(df)) {
        raw_ex <- df$exdate
        if (is.numeric(raw_ex) && mean(raw_ex, na.rm=TRUE) > 30000) { # Excel 格式
          df$exdate <- as.Date(raw_ex, origin = "1899-12-30")
        } else {
          df$exdate <- suppressWarnings(ymd(raw_ex))
        }
      }
      
      # C. 寫入 RDS (壓縮儲存)
      saveRDS(df, rds_path)
    }
    
  }, error = function(e) {
    message(paste("\n❌ 轉檔失敗:", file_name, "-", e$message))
  })
  
  setTxtProgressBar(pb, i)
}

close(pb)
message(paste("\n✅ 轉檔完成！所有檔案已儲存至:", path_target))
message("💡 下一步：請修改回測程式，將 path_opt 指向這個新資料夾。")