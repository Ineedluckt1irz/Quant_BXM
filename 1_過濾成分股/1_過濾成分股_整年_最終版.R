# ------------------------------------------------------------
# Batch filter options by DJIA constituents (Semi-Annual Supported + EK/T Fix)
# - 支援讀取 CSV 或 Excel 格式的成分股名單
# - [Updated] 支援單月(01)/雙月(02)/季度(03)/半年(06) 合併檔搜尋
# - 內建自動日期切割 (Date Slicing)
# - 特殊處理：DD, AA, MTLQ, EK, T 改用 CUSIP 直球對決 + Audit Fix 自動改名
# ------------------------------------------------------------
suppressPackageStartupMessages({
  library(data.table); library(readxl); library(dplyr); library(tidyr)
  library(stringr); library(lubridate); library(tools)
})

# ========= 路徑與年份（請依實際情況修改） =========
constituents_file <- "C:/Users/User/Desktop/Thesis_project/data_raw/DJI成分股名單.xls"
opt_dir           <- "C:/Users/User/Desktop/Thesis_project/2010"
year_target       <- 2004  
months_vec        <- sprintf("%d%02d", year_target, 1:12) 

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

# --- 日期欄位辨識 ---
date_cols <- names(con_raw)[str_detect(names(con_raw), "^[A-Za-z]{3}-\\d{2}-\\d{4}.*%$")]
if (length(date_cols) == 0) {
  date_cols <- names(con_raw)[str_detect(names(con_raw), "% Of (Equity|CSO)$")]
}
if (length(date_cols) == 0) stop("活頁簿找不到任何日期權重欄位。")

# --- Ticker 欄位辨識 ---
tick_col <- intersect(c("Ticker","ticker","Symbol","Exchange:Ticker","代號","公司代號"), names(con_raw))
if (length(tick_col) == 0) stop("找不到 Ticker 欄位。")
tick_col <- tick_col[[1]]

ticker_vec <- if (tick_col == "Exchange:Ticker") sub(".*:","", as.character(con_raw[[tick_col]])) else con_raw[[tick_col]]
ticker_vec <- clean_ticker(ticker_vec)

con_raw$row_id <- seq_len(nrow(con_raw))
map_ct <- tibble(row_id = con_raw$row_id, ticker = ticker_vec)

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

message("成分股名單處理完成，共 ", length(snap_dates), " 個快照日。")

# ========= 2) 批次處理 =========
out_dir  <- file.path(opt_dir, "成分股")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

summary_tab <- list()

for (opt_month in months_vec) {
  cat("\n==============================\n")
  cat("處理月份：", opt_month, "\n", sep = "")
  
  # 2a) 找快照
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
  
  # 2b) [Updated] 強力搜尋檔案 (支援半年彙整檔)
  options_file <- NULL
  curr_m_num   <- as.numeric(substr(opt_month, 5, 6))
  
  for (offset in 0:5) { 
    check_m_num <- curr_m_num + offset
    if (check_m_num > 12) next 
    check_month_str <- sprintf("%d%02d", year_target, check_m_num)
    cand <- list.files(opt_dir, pattern = paste0("^", check_month_str, "\\.(csv|tsv|txt|xlsx|xls)$"),
                       full.names = TRUE, ignore.case = TRUE)
    if (length(cand) > 0) {
      options_file <- cand[1]
      if (offset > 0) cat("   -> 找不到當月檔，借用後續檔案：", basename(options_file), "\n")
      break 
    }
  }
  
  if (is.null(options_file)) {
    warning("找不到檔案（嘗試搜尋至半年度底皆無）：", opt_month, "（略過）")
    summary_tab[[opt_month]] <- data.table(month=opt_month, status="Option File Missing")
    next
  }
  cat("使用檔案：", options_file, "\n", sep = "")
  
  # 2c) 讀 options 與日期切割
  opt <- read_any_file(options_file)
  
  # Date Slicing
  possible_date_cols <- grep("date", names(opt), ignore.case = TRUE, value = TRUE)
  date_col <- possible_date_cols[!grepl("ex", possible_date_cols, ignore.case = TRUE)][1]
  
  if (!is.na(date_col)) {
    opt[, temp_date_ym := format(ymd(get(date_col), quiet=TRUE), "%Y%m")]
    row_count_before <- nrow(opt)
    opt <- opt[temp_date_ym == opt_month]
    row_count_after <- nrow(opt)
    if (row_count_before != row_count_after) {
      cat("   [Date Filter] 從合併檔切分出本月資料：", row_count_before, " -> ", row_count_after, " 列\n")
    }
    opt[, temp_date_ym := NULL]
  } else {
    warning("   [Warning] 找不到 date 欄位，無法切割月份，將保留全檔。")
  }
  
  # Ticker 處理
  opt_tick_col <- grep("ticker", names(opt), ignore.case = TRUE, value = TRUE)
  if (length(opt_tick_col) == 0) {
    warning("options 檔缺少 'ticker' 欄位（略過該月）：", options_file)
    summary_tab[[opt_month]] <- data.table(month=opt_month, status="Col Ticker Missing")
    next
  }
  opt[, ticker_key := clean_ticker(get(opt_tick_col[1]))]
  
  # CUSIP 處理 (正規化)
  cusip_col <- grep("cusip", names(opt), ignore.case = TRUE, value = TRUE)
  if (length(cusip_col) > 0) {
    opt[, cusip_norm := substr(as.character(get(cusip_col[1])), 1, 8)]
  } else {
    opt[, cusip_norm := NA_character_]
  }
  
  # ============================================================
  # [Filter Logic] 模組化條件篩選
  # ============================================================
  current_ym <- as.numeric(opt_month)
  
  # 1. 定義特殊名單
  dd_aliases   <- c("DD(OLD)", "DD(Old)")
  aa_aliases   <- c("AA", "ALCOA")
  mtlq_aliases <- c("MTLQ", "MTLE")
  ek_aliases   <- c("EK")
  t_aliases    <- c("T(OLD)")
  
  # 2. 檢查當月是否有這些特殊股票
  has_dd   <- length(intersect(snap_tickers, dd_aliases)) > 0
  has_aa   <- length(intersect(snap_tickers, aa_aliases)) > 0
  has_mtlq <- length(intersect(snap_tickers, mtlq_aliases)) > 0
  has_ek   <- length(intersect(snap_tickers, ek_aliases)) > 0
  has_t    <- length(intersect(snap_tickers, t_aliases)) > 0 # [NEW!]
  
  # 3. 定義「標準成分股」(排除特殊股)
  special_set <- c()
  if(has_dd)   special_set <- c(special_set, dd_aliases)
  if(has_aa)   special_set <- c(special_set, aa_aliases)
  if(has_mtlq) special_set <- c(special_set, mtlq_aliases)
  if(has_ek)   special_set <- c(special_set, ek_aliases)
  if(has_t)    special_set <- c(special_set, t_aliases) # [NEW!]
  
  standard_tickers <- setdiff(snap_tickers, special_set)
  
  # 條件 A: 標準成分股
  cond_std <- opt$ticker_key %in% standard_tickers
  
  # 條件 B: DD (杜邦)
  cond_dd <- FALSE
  if (has_dd) {
    if (current_ym < 201709) {
      if (!all(is.na(opt$cusip_norm))) {
        cond_dd <- opt$cusip_norm == "26353410"
      } else {
        cond_dd <- opt$ticker_key %in% dd_aliases
      }
    } else if (current_ym >= 201709 & current_ym <= 201905) {
      cond_dd <- (opt$ticker_key == "DWDP") | (opt$cusip_norm == "26054310")
    } else {
      cond_dd <- opt$ticker_key %in% dd_aliases
    }
  }
  
  # 條件 C: AA (美國鋁業)
  cond_aa <- FALSE
  if (has_aa) {
    target_cusip_aa <- "44320110" 
    if (!all(is.na(opt$cusip_norm))) {
      cond_aa <- opt$cusip_norm == target_cusip_aa
      if(any(cond_aa)) cat("   [Special Filter] AA 偵測到 -> 啟用 CUSIP 鎖定:", target_cusip_aa, "\n")
    } else {
      cond_aa <- opt$ticker_key == "AA"
    }
  }
  
  # 條件 D: MTLQ (Motors Liquidation)
  cond_mtlq <- FALSE
  if (has_mtlq) {
    target_cusip_mtlq <- "62010A10"
    if (!all(is.na(opt$cusip_norm))) {
      cond_mtlq <- opt$cusip_norm == target_cusip_mtlq
      if(any(cond_mtlq)) cat("   [Special Filter] MTLQ 偵測到 -> 啟用 CUSIP 鎖定:", target_cusip_mtlq, "\n")
    } else {
      cond_mtlq <- opt$ticker_key %in% mtlq_aliases
    }
  }
  
  # 條件 E: EK (Eastman Kodak)
  cond_ek <- FALSE
  if (has_ek) {
    target_cusip_ek <- "27746110" 
    if (!all(is.na(opt$cusip_norm))) {
      cond_ek <- opt$cusip_norm == target_cusip_ek
      if(any(cond_ek)) cat("   [Special Filter] EK (Kodak) 偵測到 -> 啟用 CUSIP 鎖定:", target_cusip_ek, "\n")
    } else {
      cond_ek <- opt$ticker_key %in% ek_aliases
    }
  }
  
  # 條件 F: T (AT&T) [NEW!]
  cond_t <- FALSE
  if (has_t) {
    target_cusip_t <- "00195750"
    if (!all(is.na(opt$cusip_norm))) {
      cond_t <- opt$cusip_norm %in% target_cusip_t
      if(any(cond_t)) cat("   [Special Filter] T (AT&T) 偵測到 -> 啟用 CUSIP 鎖定 (包含 Old T/SBC)\n")
    } else {
      cond_t <- opt$ticker_key %in% t_aliases
    }
  }
  
  # 4. 綜合過濾 (OR 邏輯)
  opt_keep <- opt[cond_std | cond_dd | cond_aa | cond_mtlq | cond_ek | cond_t]
  
  # 5. [Audit Fix] 將特殊抓取的資料改名回 Excel 清單的名稱
  
  # AA 修正
  if (has_aa) {
    aa_list_name <- intersect(snap_tickers, aa_aliases)[1]
    opt_keep[cusip_norm == "01381710" | cusip_norm == "44320110", ticker_key := aa_list_name]
  }
  
  # MTLQ 修正
  if (has_mtlq) {
    mtlq_list_name <- intersect(snap_tickers, mtlq_aliases)[1]
    opt_keep[cusip_norm == "62010A10", ticker_key := mtlq_list_name]
  }
  
  # EK 修正
  if (has_ek) {
    ek_list_name <- intersect(snap_tickers, ek_aliases)[1]
    opt_keep[cusip_norm == "27746110", ticker_key := ek_list_name]
  }
  
  # T 修正 [NEW!]
  if (has_t) {
    t_list_name <- intersect(snap_tickers, t_aliases)[1]
    # 只要抓到 00195710 (Old T) 或 00206R10 (New T/SBC)，都改名為 "T"
    opt_keep[cusip_norm == "00195750" , ticker_key := t_list_name]
  }
  
  # DD 修正
  if (has_dd) {
    dd_list_name <- intersect(snap_tickers, dd_aliases)[1]
    if (current_ym >= 201709 & current_ym <= 201905) {
      opt_keep[ticker_key == "DWDP" | cusip_norm == "26054310", ticker_key := dd_list_name]
    }
    if (current_ym < 201709) {
      opt_keep[cusip_norm == "26353410", ticker_key := dd_list_name]
    }
  }
  
  # ============================================================
  
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
  
  # 2e) 稽核
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
    file_used= basename(options_file),
    kept_n   = length(kept_tickers),
    missing  = paste(missing_after, collapse = "|")
  )
}

# 最終報告
final_report <- rbindlist(summary_tab, fill = TRUE)
print(final_report)