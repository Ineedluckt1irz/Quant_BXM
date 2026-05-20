# ------------------------------------------------------------
# Batch filter options by DJIA constituents (Updated for History.csv format)
# - 支援讀取 CSV 或 Excel 格式的成分股名單
# - 自動辨識 "% Of Equity" 結尾的日期欄位
# - 批次處理指定年份的所有月份 (例如 201901 ~ 201912)
# ------------------------------------------------------------
suppressPackageStartupMessages({
  library(data.table); library(readxl); library(dplyr); library(tidyr)
  library(stringr); library(lubridate); library(tools)
})

# ========= 路徑與年份（請依實際情況修改） =========
# 注意：若檔案是 CSV，請確認副檔名正確
constituents_file <- "C:/Users/User/Desktop/Thesis_project/data_raw/DJI成分股名單.xls"
opt_dir           <- "C:/Users/User/Desktop/Thesis_project/2013"   # options 檔所在資料夾
year_target       <- 2013                                    # 目標年份
months_vec        <- sprintf("%d%02d", year_target, 1:12)    # "201901" ~ "201912"

# ========= 小工具：讀取檔案（支援 CSV/Excel） =========
read_any_file <- function(path){
  ext <- tolower(file_ext(path))
  if (ext %in% c("csv","tsv","txt")) {
    # 嘗試讀取 CSV，若失敗則嘗試 tab 分隔
    dt <- tryCatch(
      fread(path, sep = ",", showProgress = FALSE, header = TRUE),
      error = function(e) fread(path, sep = "\t", showProgress = FALSE, header = TRUE)
    )
  } else if (ext %in% c("xlsx","xls")) {
    dt <- as.data.table(read_excel(path))
  } else stop("不支援的格式：", ext)
  
  # 若 CSV 第一行是 V1, V2... 且內容像 Header，做簡易升格
  if(names(dt)[1] == "V1" && nrow(dt) > 0) {
    if(any(str_detect(as.character(dt[1,]), "Company Name"))) {
      names(dt) <- as.character(dt[1,])
      dt <- dt[-1,]
    }
  }
  dt[]
}

clean_ticker <- function(v){
  v <- toupper(trimws(as.character(v)))
  v <- sub("^.*?:","", v)            # "NYSE:AAPL" → "AAPL"
  v <- gsub("\\s+","", v)
  v
}

is_member_numpos <- function(x){
  xs <- suppressWarnings(as.numeric(x))
  !is.na(xs) & xs > 0
}

# ========= 1) 讀取 DJI 名單，建立每個【快照日】的成分集合 =========
message("正在讀取成分股名單：", constituents_file)
con_raw <- read_any_file(constituents_file)

# --- 日期欄位辨識 ---
# 抓取 "Mmm-DD-YYYY % Of Equity" 或 "CSO"
date_cols <- names(con_raw)[str_detect(names(con_raw), "^[A-Za-z]{3}-\\d{2}-\\d{4}.*%$")]
if (length(date_cols) == 0) {
  # 若 Regex 抓不到，嘗試抓結尾
  date_cols <- names(con_raw)[str_detect(names(con_raw), "% Of (Equity|CSO)$")]
}
if (length(date_cols) == 0) stop("活頁簿找不到任何日期權重欄位。")

# --- Ticker 欄位辨識 ---
tick_col <- intersect(c("Ticker","ticker","Symbol","Exchange:Ticker","代號","公司代號"), names(con_raw))
if (length(tick_col) == 0) stop("找不到 Ticker 欄位。")
tick_col <- tick_col[[1]]

# 建立對照表
ticker_vec <- if (tick_col == "Exchange:Ticker") sub(".*:","", as.character(con_raw[[tick_col]])) else con_raw[[tick_col]]
ticker_vec <- clean_ticker(ticker_vec)

# 加上 row_id 以便處理
con_raw$row_id <- seq_len(nrow(con_raw))
map_ct <- tibble(row_id = con_raw$row_id, ticker = ticker_vec)

# 轉長格式
con_long <- con_raw %>%
  select(row_id, all_of(date_cols)) %>%
  pivot_longer(cols = -row_id, names_to = "as_of", values_to = "pct") %>%
  mutate(
    snap_chr = str_remove(as_of, "\\s+% Of (Equity|CSO|Equity\\s*|CSO\\s*)$"),
    snap     = mdy(snap_chr, quiet = TRUE),
    member   = is_member_numpos(pct)
  ) %>%
  filter(!is.na(snap)) %>%
  filter(member) %>%
  left_join(map_ct, by = "row_id") %>%
  filter(!is.na(ticker), ticker != "") %>%
  transmute(ticker, snap) %>%
  distinct()

# 建立快照索引
snap_dates <- sort(unique(con_long$snap))
membership_all <- lapply(snap_dates, function(s) sort(unique(con_long$ticker[con_long$snap == s])))
names(membership_all) <- format(snap_dates, "%Y-%m-%d")

message("成分股名單處理完成，共 ", length(snap_dates), " 個快照日。")

# ========= 2) 批次處理 =========
out_dir  <- file.path(opt_dir, "成分股")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

summary_tab <- list()

for (opt_month in months_vec) {
  cat("\n==============================\n")
  cat("處理月份：", opt_month, "\n", sep = "")
  
  # 2a) 找最近且 < 該月第一天 的快照
  target_anchor <- ymd(paste0(opt_month, "01"))
  prior_idx <- suppressWarnings(max(which(snap_dates < target_anchor)))
  
  if (!is.finite(prior_idx)) {
    warning("在目標月份之前找不到任何快照日：", opt_month, "（略過）")
    summary_tab[[opt_month]] <- data.table(month=opt_month, status="No Prior Snapshot")
    next
  }
  
  snap_cutoff  <- snap_dates[prior_idx]
  snap_tickers <- membership_all[[prior_idx]]
  cat("使用快照：", as.character(snap_cutoff), "（應約 ", length(snap_tickers), " 檔）\n", sep = "")
  
  # 2b) 自動尋找該月份的 options 檔
  cand <- list.files(opt_dir, pattern = paste0("^", opt_month, "\\.(csv|tsv|txt|xlsx|xls)$"),
                     full.names = TRUE, ignore.case = TRUE)
  if (length(cand) == 0) {
    warning("找不到檔案：", file.path(opt_dir, paste0(opt_month, ".(csv|tsv|xlsx|xls)")), "（略過）")
    summary_tab[[opt_month]] <- data.table(month=opt_month, status="Option File Missing")
    next
  }
  options_file <- cand[1]
  cat("使用檔案：", options_file, "\n", sep = "")
  
  # 2c) 讀 options，過濾
  opt <- read_any_file(options_file)
  
  # 找 ticker 欄位
  opt_tick_col <- grep("ticker", names(opt), ignore.case = TRUE, value = TRUE)
  if (length(opt_tick_col) == 0) {
    warning("options 檔缺少 'ticker' 欄位（略過該月）：", options_file)
    summary_tab[[opt_month]] <- data.table(month=opt_month, status="Col Ticker Missing")
    next
  }
  
  # 建立標準化 Ticker 欄位
  opt[, ticker_key := clean_ticker(get(opt_tick_col[1]))]
  
  # 找 CUSIP 欄位 (支援 CUSIP, cusip, secid 等命名)
  cusip_col <- grep("cusip", names(opt), ignore.case = TRUE, value = TRUE)
  
  # ------------------------------------------------------------
  # 特殊處理：DD(OLD)
  # ------------------------------------------------------------
  # 設定您 Excel 中可能出現的舊杜邦名稱變體
  dd_aliases <- c( "DD(OLD)", "DD(Old)")
  
  # 檢查當月成分股名單中，是否有上述任何一個別名
  dd_target_in_snap <- intersect(snap_tickers, dd_aliases)
  has_dd_in_snap    <- length(dd_target_in_snap) > 0
  
  target_cusip_dd <- "26353410"  # 舊杜邦的 CUSIP 前8碼
  
  if (has_dd_in_snap) {
    cat("   [Special Filter] 偵測到成分股包含舊杜邦:", dd_target_in_snap, "\n")
    
    # 1. 找出「非杜邦」的其他成分股
    other_tickers <- setdiff(snap_tickers, dd_aliases)
    
    if (length(cusip_col) > 0) {
      # 確保 CUSIP 轉為字串並只取前8碼 (避免最後一碼檢查碼或格式差異)
      opt[, cusip_norm := substr(as.character(get(cusip_col[1])), 1, 8)]
      
      # 篩選邏輯： (Ticker 是其他成分股) OR (CUSIP 是 26353410)
      opt_keep <- opt[
        (ticker_key %in% other_tickers) | 
          (cusip_norm == target_cusip_dd)
      ]
      
      # [選用] 若您希望 Audit 通過，可以把抓到的舊杜邦 Ticker 強制改名回 Excel 上的名稱
      # (這步不做也可以，只是 Audit 會顯示 DD(OLD) 缺失，但實際上 DD 資料有存下來)
      # opt_keep[cusip_norm == target_cusip_dd, ticker_key := dd_target_in_snap[1]]
      
    } else {
      warning("!!! 偵測到需要抓舊杜邦，但資料檔中找不到 CUSIP 欄位！無法執行特殊篩選。")
      opt_keep <- opt[ticker_key %in% snap_tickers]
    }
    
  } else {
    # 正常模式：完全依照 Ticker 篩選
    opt_keep <- opt[ticker_key %in% snap_tickers]
  }
  # ------------------------------------------------------------
  
  # 2d) 輸出
  out_path <- file.path(out_dir, paste0(opt_month, ".csv"))
  ok <- TRUE
  tryCatch({
    fwrite(opt_keep, out_path)
  }, error = function(e){
    ok <<- FALSE
    fb <- file.path(out_dir, paste0("filtered_", opt_month, ".csv"))
    fwrite(opt_keep, fb)
    warning(paste0("原檔被占用，已改存：", fb))
  })
  
  if (ok) cat("Saved to:", out_path, "\n")
  
  # 2e) 稽核與紀錄
  kept_tickers  <- sort(unique(opt_keep$ticker_key))
  missing_after <- setdiff(snap_tickers, kept_tickers)
  
  cat("[Audit]\n",
      "- 應有成分數：", length(snap_tickers), "\n",
      "- 保留的成分數：", length(kept_tickers), "\n",
      if (length(missing_after)) paste0("- 未出現成分：", paste(missing_after, collapse = ", "), "\n") else "",
      sep = "")
  
  summary_tab[[opt_month]] <- data.table(
    month    = opt_month,
    status   = "Success",
    snapshot = as.character(snap_cutoff),
    should_n = length(snap_tickers),
    kept_n   = length(kept_tickers),
    missing  = paste(missing_after, collapse = "|")
  )
}

# 最終報告
final_report <- rbindlist(summary_tab, fill = TRUE)
print(final_report)