# ==============================================================================
# 綜合比較程式：SBXM (個股組合) vs IBXM (DIA 指數) - [修正版]
# ==============================================================================

# 1. 套件載入
library(readxl)
library(dplyr)
library(openxlsx)
library(xts)
library(PerformanceAnalytics)
library(lubridate)

# 2. 設定路徑 (請再次確認您的檔案路徑是否正確)
path_sbxm_file <- "C:/Users/User/Desktop/Thesis_project/Output(不含股利)/SBXM_Portfolio.xlsx" 
path_ibxm_file <- "C:/Users/User/Desktop/Thesis_project/Output(不含股利)/IBXM_DIA.xlsx"
path_output_dir <- "C:/Users/User/Desktop/Thesis_project/Output(不含股利)/"
output_filename <- "Comparison_SBXM_vs_IBXM.xlsx"
final_output_path <- paste0(path_output_dir, output_filename)

message("正在讀取檔案...")

# 3. 讀取資料
# ------------------------------------------------------------------------------
if(!file.exists(path_sbxm_file)) stop(paste("找不到檔案:", path_sbxm_file))
if(!file.exists(path_ibxm_file)) stop(paste("找不到檔案:", path_ibxm_file))

# [A] 讀取 Performance 表格
perf_sbxm <- read_excel(path_sbxm_file, sheet = "Performance")
perf_ibxm <- read_excel(path_ibxm_file, sheet = "Performance")

# [B] 讀取 Log 表格
log_sbxm <- read_excel(path_sbxm_file, sheet = "Portfolio_Log")
log_ibxm <- read_excel(path_ibxm_file, sheet = "Trade_Log")

# 4. 資料處理與合併
# ------------------------------------------------------------------------------

# --- [A] 合併績效指標 (Performance) ---
message("合併績效指標...")
# 使用 Metric 欄位進行合併
perf_merged <- full_join(perf_sbxm, perf_ibxm, by = "Metric")

# 重新排列欄位順序
# 自動偵測: Metric -> SBXM -> IBXM -> Benchmarks (包含 DJI Index)
cols_order <- c("Metric", 
                "SBXM Portfolio", 
                grep("IBXM", names(perf_merged), value = TRUE)[1],
                "BnH (Constituents)", 
                "Buy & Hold (DIA)", 
                "DJI Index") # 修正：這裡對應新檔案的 "DJI Index"

# 確保欄位都存在才選取
cols_order <- cols_order[cols_order %in% names(perf_merged)]
perf_final <- perf_merged[, cols_order]

message("✅ 績效指標合併完成")


# --- [B] 合併交易紀錄 (Log) 用於繪圖 ---
message("合併交易紀錄...")

# 統一日期格式
clean_date <- function(x) {
  if(is.numeric(x)) as.Date(x, origin = "1899-12-30") else as.Date(x)
}
log_sbxm$Date <- clean_date(log_sbxm$Date_For_Port)
log_ibxm$Date <- clean_date(log_ibxm$Trading_Date)
log_ibxm$Expiry <- clean_date(log_ibxm$Expiry_Date)

# 提取需要的報酬率序列
# *** 修正重點：改抓 "DJI_Return" 而不是 "DJITR_Return" ***
ts_sbxm <- log_sbxm %>% 
  select(Date, Portfolio_Return_SBXM, DJI_Return) 

ts_ibxm <- log_ibxm %>% 
  select(Date, SBXM_Return, BnH_Return) %>% 
  rename(IBXM_Return = SBXM_Return, DIA_Return = BnH_Return)

# 合併 (Inner Join 確保日期對齊)
df_merged_log <- inner_join(ts_sbxm, ts_ibxm, by = "Date") %>%
  arrange(Date) %>%
  select(Date, 
         Portfolio_Return_SBXM, 
         IBXM_Return, 
         DJI_Return, 
         DIA_Return)

message("✅ 報酬率序列合併完成")


# 5. 繪製比較圖表
# ------------------------------------------------------------------------------
# 轉換為 xts
# 使用 IBXM 的 Expiry Date 作為 X 軸
plot_dates <- log_ibxm$Expiry[match(df_merged_log$Date, log_ibxm$Date)]
ts_data <- xts(df_merged_log[, -1], order.by = plot_dates)

# 設定顯示名稱
colnames(ts_data) <- c("SBXM Portfolio (Stock Level)", 
                       "IBXM Strategy (Index Level)", 
                       "Benchmark: DJI Total Return", 
                       "DIA (ETF)")

# 顏色設定: SBXM(藍), IBXM(紅), DJI(深灰), DIA(淺灰)
my_colors <- c("blue", "red", "darkgray", "lightgray")

# 繪圖檔案
plot_file <- paste0(path_output_dir, "Comparison_Chart.png")
png(filename = plot_file, width = 1400, height = 800, res = 120)

charts.PerformanceSummary(ts_data, 
                          main = "Strategy Comparison: SBXM vs IBXM vs Benchmark", 
                          wealth.index = TRUE, 
                          colorset = my_colors, 
                          lwd = 2,
                          legend.loc = "topleft")
dev.off()


# 6. 輸出 Excel
# ------------------------------------------------------------------------------
wb <- createWorkbook()

# Sheet 1: 比較摘要 (績效表)
addWorksheet(wb, "Performance_Comparison")
writeData(wb, "Performance_Comparison", perf_final)

# Sheet 2: 合併的交易紀錄
addWorksheet(wb, "Merged_Log")
writeData(wb, "Merged_Log", df_merged_log)

# Sheet 3: 圖表
addWorksheet(wb, "Chart")
insertImage(wb, "Chart", plot_file, width = 10, height = 6, startRow = 2, startCol = 2)

# 存檔
saveWorkbook(wb, final_output_path, overwrite = TRUE)

# 清除暫存圖片
if(file.exists(plot_file)) file.remove(plot_file)

message(paste("🎉 比較檔案已建立：", final_output_path))
message("📊 包含: SBXM 與 IBXM 的並列績效表、合併報酬率紀錄、以及比較走勢圖。")