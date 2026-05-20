# ==============================================================================
# DBXM 對比分析 (Sortino Ratio) - 自動化生成報告系統
# ==============================================================================

# 1. 載入必要套件
library(readxl)
library(dplyr)
library(officer)
library(flextable)

# 2. 設定基礎路徑
base_path <- "C:/Users/User/Desktop/Thesis_project"
m_levels <- c("100", "101", "102", "103", "104", "105")

# 3. 定義邊框樣式 (學術論文標準)
std_border   <- fp_border(color = "black", width = 1.5)
thick_border <- fp_border(color = "black", width = 2.5)

# 4. 定義資料提取與 Word 生成函數
generate_comparison_report <- function(target_name, benchmark_name, output_filename) {
  df_summary_all <- data.frame()
  
  for (m in m_levels) {
    # 組合檔案路徑
    file_path <- file.path(base_path, paste0("Output_M", m), "全期間", paste0("Final_Report_M", m, ".xlsx"))
    
    if (file.exists(file_path)) {
      # 讀取績效摘要分頁
      df_perf <- read_excel(file_path, sheet = "Performance_Summary")
      
      # 篩選包含 Sortino 字樣的列
      sortino_row <- df_perf %>% filter(grepl("Sortino", .[[1]], ignore.case = TRUE))
      
      if (nrow(sortino_row) > 0) {
        # 抓取數值 (利用 contains 進行模糊匹配)
        val_target <- as.numeric(sortino_row %>% select(contains(target_name)) %>% .[1,1])
        val_bench  <- as.numeric(sortino_row %>% select(contains(benchmark_name)) %>% .[1,1])
        
        # 處理標籤
        m_label <- paste0("m=1.", ifelse(m == "100", "00", substr(m, 2, 3)))
        
        # 合併結果
        df_summary_all <- rbind(df_summary_all, data.frame(
          Moneyness = m_label,
          Target_Sortino = val_target,
          Bench_Sortino = val_bench,
          Ratio = val_target / val_bench
        ))
      }
    } else {
      warning(paste("找不到路徑中的檔案:", file_path))
    }
  }
  
  if (nrow(df_summary_all) == 0) {
    message(paste("⚠ 找不到任何資料可供提取:", target_name, "vs", benchmark_name))
    return(NULL)
  }
  
  # 建立 flextable 表格
  title_text <- paste0("Relative Sortino Ratio Efficiency: ", target_name, " vs. ", benchmark_name)
  
  ft <- flextable(df_summary_all) %>%
    add_header_lines(values = title_text) %>%
    set_header_labels(
      Moneyness      = "Moneyness 價性",
      Target_Sortino = paste(target_name, "索丁諾比率"),
      Bench_Sortino  = paste(benchmark_name, "索丁諾比率"),
      Ratio          = paste0("比率 (", target_name, "/", benchmark_name, ")")
    ) %>%
    colformat_double(digits = 3) %>%
    border_remove() %>%
    # 設定邊框
    hline_top(border = thick_border, part = "header") %>%
    hline(i = 1, border = std_border, part = "header") %>%
    hline_bottom(border = std_border, part = "header") %>%
    hline_bottom(border = thick_border, part = "body") %>%
    font(fontname = "Times New Roman", part = "all") %>%
    align(align = "center", part = "all") %>%
    bold(i = 1, part = "header") %>%
    autofit()
  
  # 寫入 Word 檔案
  output_dir <- file.path(base_path, "pictures")
  if (!dir.exists(output_dir)) { dir.create(output_dir, recursive = TRUE) }
  
  doc <- read_docx() %>% body_add_flextable(value = ft)
  export_path <- file.path(output_dir, output_filename)
  print(doc, target = export_path)
  
  message(paste("✅ 報告已生成：", output_filename))
}

# 5. 執行生成 (針對 DBXM 的需求)
# ==========================================

# 定義要產生的對照組
dbxm_tasks <- list(
  list("DBXM", "CBXM", "DBXM_vs_CBXM_Sortino_Analysis.docx"),
  list("DBXM", "MBXM", "DBXM_vs_MBXM_Sortino_Analysis.docx")
)

# 執行循環
for (task in dbxm_tasks) {
  generate_comparison_report(task[[1]], task[[2]], task[[3]])
}

message("\n--- DBXM 相關報告處理完畢 ---")