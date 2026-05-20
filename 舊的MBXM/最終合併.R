# 1. 套件載入 =====
library(readxl)
library(openxlsx)
library(dplyr)
library(purrr)
library(xts)
library(PerformanceAnalytics)

# 2. 設定區  =====
base_path <- "C:/Users/User/Desktop/Thesis_project"
paths <- list(
  SBXM = file.path(base_path, "Output_SBXM/SBXM_Portfolio.xlsx"),
  IBXM = file.path(base_path, "Output_IBXM/IBXM_DIA.xlsx"),
  CBXM = file.path(base_path, "Output_CBXM/CBXM_Portfolio.xlsx"),
  MBXM = file.path(base_path, "Output_30D_5_V2/Output_MBXM/MBXM_Portfolio.xlsx"),
  DBXM = file.path(base_path, "Output_30D_5_V2/Output_DBXM/DBXM_Portfolio.xlsx")
)

# --- 在這裡修改您想要的顯示名稱 ---
display_names <- c(
  BnH          = "BnH(constitute)",               
  SBXM         = "SBXM(M=1)",          
  CBXM         = "CBXM(M=1)",     
  DIA          = "DIA(ETF)",         
  IBXM         = "IBXM(M=1)",        
  CIBXM        = "C-IBXM(M=1)",       
  MBXM         = "MBXM(M=1)",          
  DBXM         = "DBXM(M=1)",        
  DJI          = "DJITR"               
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
  if(name == "SBXM")  colnames(df)[grepl("SBXM", colnames(df))] <- display_names["SBXM"]
  if(name == "IBXM") {
    colnames(df)[colnames(df) == "IBXM (M=1)"]  <- display_names["IBXM"]
    colnames(df)[colnames(df) == "C-IBXM (VIX)"] <- display_names["CIBXM"]
  }
  if(name == "CBXM")  colnames(df)[grepl("CBXM", colnames(df))] <- display_names["CBXM"]
  if(name == "MBXM")  colnames(df)[grepl("MBXM", colnames(df))] <- display_names["MBXM"]
  if(name == "DBXM")  colnames(df)[grepl("DBXM", colnames(df))] <- display_names["DBXM"]
  
  bench_keywords <- "BnH|DJITR|DJI|Market|DIA"
  strat_cols <- colnames(df)[!grepl(bench_keywords, colnames(df)) | colnames(df) == "Metric"]
  return(df[, strat_cols])
}

list_perf <- imap(paths, ~load_and_standardize_perf(.x, .y))
df_perf_strat <- reduce(list_perf, full_join, by = "Metric")

# 處理基準指標名
df_sbxm_bench <- read_excel(paths$SBXM, sheet = "Performance")
colnames(df_sbxm_bench)[1] <- "Metric"
df_sbxm_bench$Metric <- gsub("Worst Drawdown", "Max Drawdown", df_sbxm_bench$Metric)
df_sbxm_bench$Metric <- gsub("Sortino Ratio \\(MAR = 0%\\)", "Sortino Ratio", df_sbxm_bench$Metric)

# 基準更名
df_bench_final <- df_sbxm_bench %>% 
  select(Metric, BnH, `DIA(ETF)`, DJITR) %>%
  rename(!!display_names["BnH"] := BnH, !!display_names["DIA"] := `DIA(ETF)`, !!display_names["DJI"] := DJITR)

df_perf_output <- full_join(df_perf_strat, df_bench_final, by = "Metric") %>%
  select(Metric, intersect(final_order, colnames(.))) %>%
  filter(!is.na(Metric)) %>% distinct(Metric, .keep_all = TRUE)

# 4. 月報酬率補齊與更名 =====
load_ret_and_rename <- function(p, name) {
  s_name <- if(name == "IBXM") "Trade_Log" else "Portfolio_Log"
  df <- read_excel(p, sheet = s_name)
  date_col <- names(df)[grepl("Date|Trading", names(df))][1]
  
  if(name == "SBXM")  df <- df %>% rename(!!display_names["SBXM"] := Portfolio_Return_SBXM)
  if(name == "IBXM")  df <- df %>% rename(!!display_names["IBXM"] := IBXM_Return, !!display_names["CIBXM"] := CIBXM_Return)
  if(name == "CBXM")  df <- df %>% rename(!!display_names["CBXM"] := Portfolio_Return_CBXM)
  if(name == "MBXM")  df <- df %>% rename(!!display_names["MBXM"] := Portfolio_Return_MBXM)
  if(name == "DBXM")  df <- df %>% rename(!!display_names["DBXM"] := DBXM_Return)
  
  df_sub <- df[, intersect(colnames(df), c(date_col, final_order))]
  colnames(df_sub)[1] <- "Date"; df_sub$Date <- as.Date(df_sub$Date)
  return(df_sub)
}

list_ret_strat <- imap(paths, ~load_ret_and_rename(.x, .y))
df_ret_merged <- reduce(list_ret_strat, full_join, by = "Date")

df_sbxm_log <- read_excel(paths$SBXM, sheet = "Portfolio_Log")
df_bench_ret <- df_sbxm_log %>% 
  select(Date_For_Port, Portfolio_Return_BnH, DIA_Return, DJITR_Return) %>%
  rename(Date = Date_For_Port, !!display_names["BnH"] := Portfolio_Return_BnH, 
         !!display_names["DIA"] := DIA_Return, !!display_names["DJI"] := DJITR_Return) %>%
  mutate(Date = as.Date(Date))

df_ret_output <- full_join(df_ret_merged, df_bench_ret, by = "Date") %>% 
  arrange(Date) %>% select(Date, intersect(final_order, colnames(.)))

# 5. 生成圖表與 Excel 匯出 =====
ts_ret <- xts(df_ret_output[,-1], order.by = df_ret_output$Date)
ts_ret[is.na(ts_ret)] <- 0 

# 原本定義的完整顏色清單
my_colors <- c("black", "red", "darkorange", "#778899", "blue", "goldenrod", "darkgreen", "purple", "gray")

# --- 圖表 1：全策略與基準比較 ---
temp_img_all <- tempfile(fileext = ".png")
png(temp_img_all, width = 1200, height = 700, res = 120)
chart.CumReturns(ts_ret, main = "Strategy Comparison: All", wealth.index = TRUE, 
                 lwd = 2, legend.loc = "topleft", colorset = my_colors)
dev.off()

# --- 圖表 2：僅包含這六者 (維持原色) ---
target_m1 <- c("SBXM(M=1)", "CBXM(M=1)", "IBXM(M=1)", "C-IBXM(M=1)", "MBXM(M=1)", "DBXM(M=1)")

# 確保只選取現有的欄位並保持順序
available_m1 <- intersect(target_m1, colnames(ts_ret))
ts_ret_m1 <- ts_ret[, available_m1]

# 從原顏色清單中對應出這六個標籤的顏色
# 根據你的 display_names 順序：SBXM(2), CBXM(3), IBXM(5), CIBXM(6), MBXM(7), DBXM(8)
m1_colors <- my_colors[match(available_m1, final_order)]

temp_img_m1 <- tempfile(fileext = ".png")
png(temp_img_m1, width = 1200, height = 700, res = 120)
chart.CumReturns(ts_ret_m1, main = "Strategy Comparison: M=1 Group", wealth.index = TRUE, 
                 lwd = 2, legend.loc = "topleft", colorset = m1_colors)
dev.off()

# --- Excel 匯出 ---
wb <- createWorkbook()
addWorksheet(wb, "Performance_Summary"); writeData(wb, "Performance_Summary", df_perf_output)
addWorksheet(wb, "Monthly_Returns"); writeData(wb, "Monthly_Returns", df_ret_output)
addWorksheet(wb, "Wealth_Index"); writeData(wb, "Wealth_Index", data.frame(Date = index(ts_ret), coredata(cumprod(1 + ts_ret))))

# 分頁 1: 放在原本的 Comparison_Chart 位置 (全圖)
addWorksheet(wb, "Comparison_Chart")
insertImage(wb, "Comparison_Chart", temp_img_all, width = 12, height = 7, startRow = 2, startCol = 2)

# 分頁 2: 新增 M1 專屬圖表
addWorksheet(wb, "Comparison_M1_Only")
insertImage(wb, "Comparison_M1_Only", temp_img_m1, width = 12, height = 7, startRow = 2, startCol = 2)

saveWorkbook(wb, path_final_report, overwrite = TRUE)
message("✅ 任務完成！顏色已對齊原始設定。")