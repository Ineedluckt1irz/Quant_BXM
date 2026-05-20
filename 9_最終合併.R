# ==========================================
# Final Report 合併 + 圖表輸出 (修正版：支援 MBXM= MABXM 命名 + 動態 Moneyness)
# ==========================================

# 1. 套件載入 =====
library(readxl)
library(openxlsx)
library(dplyr)
library(purrr)
library(xts)
library(PerformanceAnalytics)

# 2. 設定區  =====
moneyness <- 1.00   #要改
base_path <- "C:/Users/User/Desktop/Thesis_project"
paths <- list(
  SBXM = file.path(base_path, "Output_SBXM/SBXM_Portfolio.xlsx"),
  IBXM = file.path(base_path, "Output_IBXM/IBXM_DIA.xlsx"),
  CBXM = file.path(base_path, "Output_CBXM/CBXM_Portfolio.xlsx"),
  MBXM = file.path(base_path, "Output_MABXM/MABXM_Portfolio.xlsx"),
  DBXM = file.path(base_path, "Output_DBXM/DBXM_VIX_MA_Portfolio_Final.xlsx")
)

# --- [修改] 動態顯示名稱 ---
# 使用 paste0 將 moneyness 變數帶入
display_names <- c(
  BnH   = "BnH(constitute)",
  SBXM  = paste0("SBXM(M=", moneyness, ")"),
  CBXM  = paste0("CBXM(M=", moneyness, ")"),
  DIA   = "DIA(ETF)",
  IBXM  = paste0("IBXM(M=", moneyness, ")"),
  CIBXM = paste0("C-IBXM(M=", moneyness, ")"),
  MBXM  = paste0("MBXM(M=", moneyness, ")"),
  DBXM  = paste0("DBXM(M=", moneyness, ")"),
  DJI   = "DJITR"
)

# 最終排序順序 (使用上面的 key 名稱)
final_order <- as.character(display_names)

path_final_report <- file.path(base_path, "Final_Report.xlsx")

# 3. 績效指標處理與合併 =====
load_and_standardize_perf <- function(p, name) {
  df <- read_excel(p, sheet = "Performance")
  colnames(df)[1] <- "Metric"
  df$Metric <- gsub("Worst Drawdown", "Max Drawdown", df$Metric)
  df$Metric <- gsub("Sortino Ratio \\(MAR = 0%\\)", "Sortino Ratio", df$Metric)
  
  # 根據檔案類型重新命名策略欄位
  if (name == "SBXM") {
    colnames(df)[grepl("SBXM", colnames(df))] <- display_names["SBXM"]
  }
  
  if (name == "IBXM") {
    # [修改] 這裡也要改成動態判斷輸入檔的欄位名稱
    target_ibxm_col <- paste0("IBXM (M=", moneyness, ")")
    target_cibxm_col <- paste0("C-IBXM (M=", moneyness, ")")
    
    if (target_ibxm_col %in% colnames(df)) colnames(df)[colnames(df) == target_ibxm_col] <- display_names["IBXM"]
    if ("C-IBXM (VIX)" %in% colnames(df)) colnames(df)[colnames(df) == "C-IBXM (VIX)"] <- display_names["CIBXM"]
    if (target_cibxm_col %in% colnames(df)) colnames(df)[colnames(df) == target_cibxm_col] <- display_names["CIBXM"]
  }
  
  if (name == "CBXM") {
    colnames(df)[grepl("CBXM", colnames(df))] <- display_names["CBXM"]
  }
  
  if (name == "MBXM") {
    # 修正重點：MBXM 檔案實際可能叫 MABXM (維持原本邏輯)
    hit <- colnames(df)[grepl("MABXM|MBXM", colnames(df), ignore.case = TRUE)]
    if (length(hit) > 0) colnames(df)[match(hit[1], colnames(df))] <- display_names["MBXM"]
  }
  
  if (name == "DBXM") {
    colnames(df)[grepl("DBXM", colnames(df))] <- display_names["DBXM"]
  }
  
  # 只保留策略欄位（把基準欄位丟掉），Metric 例外保留
  bench_keywords <- "BnH|DJITR|DJI|Market|DIA"
  strat_cols <- colnames(df)[(!grepl(bench_keywords, colnames(df))) | colnames(df) == "Metric"]
  
  df <- df[, strat_cols, drop = FALSE]
  df
}

list_perf <- imap(paths, ~load_and_standardize_perf(.x, .y))
df_perf_strat <- reduce(list_perf, full_join, by = "Metric")

# 基準指標名（從 SBXM 的 Performance 抓）
df_sbxm_bench <- read_excel(paths$SBXM, sheet = "Performance")
colnames(df_sbxm_bench)[1] <- "Metric"
df_sbxm_bench$Metric <- gsub("Worst Drawdown", "Max Drawdown", df_sbxm_bench$Metric)
df_sbxm_bench$Metric <- gsub("Sortino Ratio \\(MAR = 0%\\)", "Sortino Ratio", df_sbxm_bench$Metric)

# 基準更名
df_bench_final <- df_sbxm_bench %>%
  select(Metric, BnH, `DIA(ETF)`, DJITR) %>%
  rename(
    !!display_names["BnH"] := BnH,
    !!display_names["DIA"] := `DIA(ETF)`,
    !!display_names["DJI"] := DJITR
  )

df_perf_output <- full_join(df_perf_strat, df_bench_final, by = "Metric") %>%
  select(Metric, intersect(final_order, colnames(.))) %>%
  filter(!is.na(Metric)) %>%
  distinct(Metric, .keep_all = TRUE)

# 4. 月報酬率補齊與更名 =====
load_ret_and_rename <- function(p, name) {
  s_name <- if (name == "IBXM") "Trade_Log" else "Portfolio_Log"
  df <- read_excel(p, sheet = s_name)
  
  # 日期欄位自動抓
  date_col <- names(df)[grepl("Date|Trading", names(df), ignore.case = TRUE)][1]
  if (is.na(date_col)) stop("找不到日期欄位：", name, " / sheet=", s_name)
  
  # SBXM
  if (name == "SBXM") {
    if ("Portfolio_Return_SBXM" %in% names(df)) {
      df <- df %>% rename(!!display_names["SBXM"] := Portfolio_Return_SBXM)
    } else {
      stop("SBXM 缺少欄位 Portfolio_Return_SBXM")
    }
  }
  
  # IBXM & CIBXM
  if (name == "IBXM") {
    if ("IBXM_Return" %in% names(df)) df <- df %>% rename(!!display_names["IBXM"] := IBXM_Return)
    if ("CIBXM_Return" %in% names(df)) df <- df %>% rename(!!display_names["CIBXM"] := CIBXM_Return)
    if (!(display_names["IBXM"] %in% names(df))) stop("IBXM 缺少欄位 IBXM_Return")
    if (!(display_names["CIBXM"] %in% names(df))) stop("IBXM 缺少欄位 CIBXM_Return")
  }
  
  # CBXM
  if (name == "CBXM") {
    if ("Portfolio_Return_CBXM" %in% names(df)) {
      df <- df %>% rename(!!display_names["CBXM"] := Portfolio_Return_CBXM)
    } else {
      stop("CBXM 缺少欄位 Portfolio_Return_CBXM")
    }
  }
  
  # MBXM（修正重點：檔案可能是 MABXM_Return）
  if (name == "MBXM") {
    cand <- c("Portfolio_Return_MBXM", "MABXM_Return", "MBXM_Return", "Portfolio_Return_MABXM")
    hit <- intersect(cand, names(df))
    if (length(hit) == 0) {
      stop("MBXM 檔案找不到報酬率欄位，候選：", paste(cand, collapse = ", "))
    }
    df <- df %>% rename(!!display_names["MBXM"] := all_of(hit[1]))
  }
  
  # DBXM
  if (name == "DBXM") {
    if ("DBXM_Return" %in% names(df)) {
      df <- df %>% rename(!!display_names["DBXM"] := DBXM_Return)
    } else {
      stop("DBXM 缺少欄位 DBXM_Return")
    }
  }
  
  # 只保留日期 + 策略欄位
  keep_cols <- intersect(colnames(df), c(date_col, final_order))
  df_sub <- df[, keep_cols, drop = FALSE]
  
  # 統一日期欄
  colnames(df_sub)[1] <- "Date"
  df_sub$Date <- as.Date(df_sub$Date)
  
  df_sub
}

list_ret_strat <- imap(paths, ~load_ret_and_rename(.x, .y))
df_ret_merged <- reduce(list_ret_strat, full_join, by = "Date")

# 基準月報酬（從 SBXM 的 Portfolio_Log 抓）
df_sbxm_log <- read_excel(paths$SBXM, sheet = "Portfolio_Log")
df_bench_ret <- df_sbxm_log %>%
  select(Date_For_Port, Portfolio_Return_BnH, DIA_Return, DJITR_Return) %>%
  rename(
    Date = Date_For_Port,
    !!display_names["BnH"] := Portfolio_Return_BnH,
    !!display_names["DIA"] := DIA_Return,
    !!display_names["DJI"] := DJITR_Return
  ) %>%
  mutate(Date = as.Date(Date))

df_ret_output <- full_join(df_ret_merged, df_bench_ret, by = "Date") %>%
  arrange(Date) %>%
  select(Date, intersect(final_order, colnames(.)))

# 5. 生成圖表與 Excel 匯出 =====
ts_ret <- xts(df_ret_output[, -1], order.by = df_ret_output$Date)
ts_ret[is.na(ts_ret)] <- 0

# 原本定義的完整顏色清單
my_colors <- c("black", "red", "darkorange", "#778899", "blue", "goldenrod", "darkgreen", "purple", "gray")

# 圖表 1：全策略與基準比較
temp_img_all <- tempfile(fileext = ".png")
png(temp_img_all, width = 1200, height = 700, res = 120)
chart.CumReturns(
  ts_ret,
  main = "Strategy Comparison: All",
  wealth.index = TRUE,
  lwd = 2,
  legend.loc = "topleft",
  colorset = my_colors
)
dev.off()

# 圖表 2：僅 Specific M Group
# [修改] 使用動態名稱建立 target 清單
target_m_subset <- c(
  display_names["SBXM"], 
  display_names["CBXM"], 
  display_names["IBXM"], 
  display_names["CIBXM"], 
  display_names["MBXM"], 
  display_names["DBXM"]
)
available_m_subset <- intersect(target_m_subset, colnames(ts_ret))
ts_ret_subset <- ts_ret[, available_m_subset]

# 對應顏色（用 final_order 對照 my_colors）
subset_colors <- my_colors[match(available_m_subset, final_order)]

# [修改] 動態標題
plot_title_subset <- paste0("Strategy Comparison: M=", moneyness, " Group")

temp_img_subset <- tempfile(fileext = ".png")
png(temp_img_subset, width = 1200, height = 700, res = 120)
chart.CumReturns(
  ts_ret_subset,
  main = plot_title_subset,
  wealth.index = TRUE,
  lwd = 2,
  legend.loc = "topleft",
  colorset = subset_colors
)
dev.off()

# Excel 匯出
wb <- createWorkbook()

addWorksheet(wb, "Performance_Summary")
writeData(wb, "Performance_Summary", df_perf_output)

addWorksheet(wb, "Monthly_Returns")
writeData(wb, "Monthly_Returns", df_ret_output)

addWorksheet(wb, "Wealth_Index")
writeData(wb, "Wealth_Index", data.frame(Date = index(ts_ret), coredata(cumprod(1 + ts_ret))))

addWorksheet(wb, "Comparison_Chart")
insertImage(wb, "Comparison_Chart", temp_img_all, width = 12, height = 7, startRow = 2, startCol = 2)

# [修改] 讓 Sheet 名稱比較通用或包含變數 (避免寫死 M1)
sheet_name_subset <- paste0("Comparison_M_Only") 
addWorksheet(wb, sheet_name_subset)
insertImage(wb, sheet_name_subset, temp_img_subset, width = 12, height = 7, startRow = 2, startCol = 2)

saveWorkbook(wb, path_final_report, overwrite = TRUE)
message("✅ 任務完成！")