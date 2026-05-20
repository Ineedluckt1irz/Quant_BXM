# ------------------------------------------------------------
# Filter options by DJIA constituents (Single Month Version)
# - 支援讀取 CSV 或 Excel 格式的成分股名單
# - 自動辨識 "% Of Equity" 結尾的日期欄位
# - 內建 DuPont Merger (DD/DWDP) 自動切換邏輯
# ------------------------------------------------------------
suppressPackageStartupMessages({
  library(data.table); library(readxl); library(dplyr); library(tidyr)
  library(stringr); library(lubridate); library(tools)
})

# ========= 路徑與月份（請依實際情況修改） =========
constituents_file <- "C:/Users/User/Desktop/Thesis_project/data_raw/DJI成分股名單.xls"
opt_dir           <- "C:/Users/User/Desktop/Thesis_project/2017"
opt_month         <- "201709"  # 目標月份

# ========= 自動尋找該月份的 Options 檔 =========
cand <- list.files(opt_dir, pattern = paste0("^", opt_month, "\\.(csv|tsv|txt|xlsx|xls)$"), 
                   full.names = TRUE, ignore.case = TRUE)
if (length(cand) == 0) stop("找不到檔案：", file.path(opt_dir, paste0(opt_month, ".(csv|tsv|xlsx|xls)")))
options_file <- cand[1]
message("使用 Options 檔案：", options_file)

# ========= 小工具：讀取檔案（支援 CSV/Excel） =========
read_any_file <- function(path){
  ext <- tolower(file_ext(path))
  if (ext %in% c("csv","tsv","txt")) {
    dt <- tryCatch(
      fread(path, sep = ",", showProgress = FALSE, header = TRUE),
      error = function(e) fread(path, sep = "\t", showProgress = FALSE, header = TRUE)
    )
  } else if (ext %in% c("xlsx","xls")) {
    dt <- as.data.table(read_excel(path))
  } else stop("不支援的格式：", ext)
  
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
  v <- sub("^.*?:","", v)            
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

# --- 日期欄位辨識規則 ---
date_cols <- names(con_raw)[str_detect(names(con_raw), "^[A-Za-z]{3}-\\d{2}-\\d{4}.*%$")]
if (length(date_cols) == 0) {
  date_cols <- names(con_raw)[str_detect(names(con_raw), "% Of Equity$")]
}
if (length(date_cols) == 0) stop("活頁簿找不到任何日期權重欄位。")

message("偵測到 ", length(date_cols), " 個歷史快照欄位。")

# --- Ticker 欄位 ---
tick_col <- intersect(c("Ticker","ticker","Symbol","Exchange:Ticker","代號","公司代號"), names(con_raw))
if (length(tick_col) == 0) stop("找不到 Ticker 欄位。")
tick_col <- tick_col[[1]]

ticker_vec <- if (tick_col == "Exchange:Ticker") sub(".*:","", as.character(con_raw[[tick_col]])) else con_raw[[tick_col]]
ticker_vec <- clean_ticker(ticker_vec)

map_ct <- tibble(row_id = seq_len(nrow(con_raw)), ticker = ticker_vec)
con_raw$row_id <- seq_len(nrow(con_raw))

# --- Pivot 與清理 ---
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

snap_dates <- sort(unique(con_long$snap))
membership_all <- lapply(snap_dates, function(s) sort(unique(con_long$ticker[con_long$snap == s])))
names(membership_all) <- format(snap_dates, "%Y-%m-%d")

# ========= 2) 目標月份 → 取「小於該月份第一天」的最近快照 =========
target_anchor <- ymd(paste0(opt_month, "01")) 
prior_idx <- max(which(snap_dates < target_anchor))

if (length(prior_idx) == 0 || is.infinite(prior_idx)) {
  stop("在目標月份 (", opt_month, ") 之前找不到任何成分股快照日。")
}

snap_cutoff  <- snap_dates[prior_idx]
snap_tickers <- membership_all[[prior_idx]]

message("目標月份：", opt_month, 
        " | 基準日：", target_anchor,
        "\n使用最近快照日：", as.character(snap_cutoff), 
        " (成分股數量：", length(snap_tickers), ")")

# ========= 3) 讀 options，過濾 (整合 DD/DWDP 邏輯) =========
opt <- read_any_file(options_file)

if (!any(grepl("ticker", names(opt), ignore.case = TRUE))) {
  stop("options 檔需包含 'ticker' 欄位。")
}
opt_tick_col <- grep("ticker", names(opt), ignore.case = TRUE, value = TRUE)[1]
opt[, ticker_key := clean_ticker(get(opt_tick_col))]

# 偵測 CUSIP 欄位 (供 DD 判斷使用)
cusip_col <- grep("cusip", names(opt), ignore.case = TRUE, value = TRUE)

# ------------------------------------------------------------
# [New Logic] DD (Old) vs DowDuPont 自動切換
# ------------------------------------------------------------
dd_aliases <- c("DD(OLD)", "DD(Old)") # Excel 可能出現的名字
dd_target_in_snap <- intersect(snap_tickers, dd_aliases)
has_dd_in_snap    <- length(dd_target_in_snap) > 0

current_ym <- as.numeric(opt_month) # 將 "201709" 轉為數字 201709

if (has_dd_in_snap) {
  # 1. 排除 DD 以免混淆
  other_tickers <- setdiff(snap_tickers, dd_aliases)
  
  if (current_ym < 201709) {
    # === [時期 A] < 201709：抓舊杜邦 (Old DD) ===
    cat(">>> [Filter] 偵測到 DD (Old Period) -> 鎖定 CUSIP: 26353410\n")
    target_cusip <- "26353410"
    
    if (length(cusip_col) > 0) {
      opt[, cusip_norm := substr(as.character(get(cusip_col[1])), 1, 8)]
      opt_keep <- opt[(ticker_key %in% other_tickers) | (cusip_norm == target_cusip)]
      
      # (選用) 將抓到的舊杜邦改名回 Excel 上的名字
      # opt_keep[cusip_norm == target_cusip, ticker_key := dd_target_in_snap[1]]
    } else {
      warning("需抓舊杜邦但無 CUSIP 欄位，退回僅用 Ticker 篩選。")
      opt_keep <- opt[ticker_key %in% snap_tickers]
    }
    
  } else if (current_ym >= 201709 & current_ym <= 201905) {
    # === [時期 B] 201709 ~ 201905：抓 DowDuPont (DWDP) ===
    cat(">>> [Filter] 偵測到 DD (Merger Period) -> 自動轉抓 DWDP / CUSIP: 26054310\n")
    
    target_ticker_new <- "DWDP"
    target_cusip_new  <- "26054310"
    
    is_other <- opt$ticker_key %in% other_tickers
    is_dwdp  <- opt$ticker_key == target_ticker_new
    
    if (length(cusip_col) > 0) {
      opt[, cusip_norm := substr(as.character(get(cusip_col[1])), 1, 8)]
      is_dwdp_cusip <- opt$cusip_norm == target_cusip_new
      opt_keep <- opt[is_other | is_dwdp | is_dwdp_cusip]
    } else {
      opt_keep <- opt[is_other | is_dwdp]
    }
    
    # [關鍵步驟] 將抓到的 DWDP 改名回 Excel 裡的名字 (例如 DD(OLD))
    found_dwdp_idx <- which(!opt_keep$ticker_key %in% other_tickers)
    if (length(found_dwdp_idx) > 0) {
      cat(">>> 已將", length(found_dwdp_idx), "筆 DWDP 資料重新標記為", dd_target_in_snap[1], "\n")
      opt_keep[found_dwdp_idx, ticker_key := dd_target_in_snap[1]]
    }
    
  } else {
    # === [時期 C] > 201905：正常抓 DD ===
    opt_keep <- opt[ticker_key %in% snap_tickers]
  }
  
} else {
  # 名單無 DD，正常處理
  opt_keep <- opt[ticker_key %in% snap_tickers]
}
# ------------------------------------------------------------

# ========= 4) 輸出 =========
out_dir  <- file.path(opt_dir, "成分股")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(out_dir, paste0(opt_month, ".csv"))

tryCatch({
  fwrite(opt_keep, out_path)
  cat("已儲存過濾檔案至：", out_path, "\n")
}, error = function(e){
  fb <- file.path(out_dir, paste0("filtered_", opt_month, ".csv"))
  fwrite(opt_keep, fb)
  warning(paste0("原檔名無法寫入，已改存：", fb))
})

# ========= 5) 簡易稽核 =========
kept_tickers  <- sort(unique(opt_keep$ticker_key))
missing_after <- setdiff(snap_tickers, kept_tickers)

cat("\n[Audit Result]\n")
cat("1. 應有成分股 (", length(snap_tickers), "): ", paste(head(snap_tickers, 5), collapse=","), "...\n", sep="")
cat("2. 實際 options 包含 (", length(kept_tickers), "): ", paste(head(kept_tickers, 5), collapse=","), "...\n", sep="")
if (length(missing_after) > 0) {
  cat("3. 未出現在 options 檔的成分股 (", length(missing_after), "): ", paste(missing_after, collapse = ", "), "\n", sep="")
} else {
  cat("3. 完美！所有預期成分股皆在 options 檔中找到。\n")
}