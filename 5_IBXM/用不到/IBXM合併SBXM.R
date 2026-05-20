# ==============================================================================
# 綜合比較程式：SBXM (個股組合) vs IBXM (DIA 指數)
# ==============================================================================

# 1. 套件載入
library(readxl)
library(dplyr)
library(openxlsx)
library(xts)
library(PerformanceAnalytics)
library(lubridate)

# 2. 設定路徑 (根據您提供的路徑)
path_sbxm_file <- "C:/Users/User/Desktop/Thesis_project/Output(含股利)/SBXM_Portfolio_DJITR_Benchmark.xlsx"
path_ibxm_file <- "C:/Users/User/Desktop/Thesis_project/Output(含股利)/IBXM_DIA.xlsx"
path_output_dir <- "C:/Users/User/Desktop/Thesis_project/Output(含股利)/"
output_filename <- "Comparison_SBXM_vs_IBXM.xlsx"
final_output_path <- paste0(path_output_dir, output_filename)

message("正在讀取檔案...")

# 3. 讀取資料
# ------------------------------------------------------------------------------
# [A] 讀取 Performance 表格
perf_sbxm <- read_excel(path_sbxm_file, sheet = "Performance")
perf_ibxm <- read_excel(path_ibxm_file, sheet = "Performance")

# [B] 讀取 Log 表格
log_sbxm <- read_excel(path_sbxm_file, sheet = "Portfolio_Log")
log_ibxm <- read_excel(path_ibxm_file, sheet = "Trade_Log")

# 4. 資料處理與合併
# ------------------------------------------------------------------------------

# --- [A] 合併績效指標 (Performance) ---
# 使用 Metric 欄位進行合併 (Full Join 以防欄位不一致)
# 確保 IBXM 的欄位名稱清楚 (原本可能是 "IBXM (M=1)")
perf_merged <- full_join(perf_sbxm, perf_ibxm, by = "Metric")

# 重新排列欄位順序，方便閱讀: Metric -> SBXM -> IBXM -> Benchmarks
# 這裡自動偵測欄位名稱進行排序
cols_order <- c("Metric", 
                "SBXM Portfolio", 
                grep("IBXM", names(perf_merged), value = TRUE)[1], # 自動抓取 IBXM 名稱
                "BnH (Constituents)", 
                "Buy & Hold (DIA)", 
                "DJI (Total Return)")
# 確保欄位都存在才選取
cols_order <- cols_order[cols_order %in% names(perf_merged)]
perf_final <- perf_merged[, cols_order]

message("✅ 績效指標合併完成")


# --- [B] 合併交易紀錄 (Log) 用於繪圖 ---
# SBXM 關鍵欄位: Date_For_Port, Expiry_Date_Ref, Portfolio_Return_SBXM, DJITR_Return
# IBXM 關鍵欄位: Trading_Date,  Expiry_Date,     SBXM_Return (這是IBXM策略), BnH_Return (這是DIA)

# 統一日期格式
clean_date <- function(x) as.Date(x, origin = "1899-12-30")
log_sbxm$Date <- clean_date(log_sbxm$Date_For_Port)
log_ibxm$Date <- clean_date(log_ibxm$Trading_Date)
log_ibxm$Expiry <- clean_date(log_ibxm$Expiry_Date)

# 提取需要的報酬率序列
ts_sbxm <- log_sbxm %>% select(Date, Portfolio_Return_SBXM, DJITR_Return)
ts_ibxm <- log_ibxm %>% select(Date, SBXM_Return, BnH_Return)

# 為了避免混淆，將 IBXM 檔內的 "SBXM_Return" 改名為 "IBXM_Return"
ts_ibxm <- ts_ibxm %>% rename(IBXM_Return = SBXM_Return, DIA_Return = BnH_Return)

# 合併 (Inner Join 確保日期對齊)
df_merged_log <- inner_join(ts_sbxm, ts_ibxm, by = "Date") %>%
  arrange(Date) %>%
  select(Date, 
         Portfolio_Return_SBXM, 
         IBXM_Return, 
         DJITR_Return, 
         DIA_Return)

message("✅ 報酬率序列合併完成")


# 5. 繪製比較圖表
# ------------------------------------------------------------------------------
# 轉換為 xts
ts_data <- xts(df_merged_log[, -1], order.by = log_ibxm$Expiry[match(df_merged_log$Date, log_ibxm$Date)]) # 使用到期日作為 X 軸

# 設定顯示名稱
colnames(ts_data) <- c("SBXM Portfolio (Stock Level)", 
                       "IBXM Strategy (Index Level)", 
                       "Benchmark: DJI Total Return", 
                       "DIA (ETF)")

# 顏色設定: SBXM(藍), IBXM(紅), DJI(灰), DIA(淺灰)
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