# ==========================================
# CBXM 對比分析 (Sortino Ratio Ratio) - 自動化生成報告
# ==========================================

# 1. 載入必要套件
library(readxl)
library(dplyr)
library(officer)
library(flextable)

# 2. 設定基礎路徑
base_path <- "C:/Users/User/Desktop/Thesis_project"
m_levels <- c("100", "101", "102", "103", "104", "105")

# 3. 定義邊框樣式
std_border <- fp_border(color = "black", width = 1.5)
thick_border <- fp_border(color = "black", width = 2.5)

# 4. 資料提取與 Word 生成函數
generate_comparison_report <- function(target_name, benchmark_name, output_filename) {
  df_summary_all <- data.frame()
  
  for (m in m_levels) {
    file_path <- file.path(base_path, paste0("Output_M", m), "全期間", paste0("Final_Report_M", m, ".xlsx"))
    
    if (file.exists(file_path)) {
      df_perf <- read_excel(file_path, sheet = "Performance_Summary")
      sortino_row <- df_perf %>% filter(grepl("Sortino", .[[1]]))
      
      # 模糊匹配抓取目標策略數值
      val_target <- as.numeric(sortino_row %>% select(contains(target_name)) %>% .[1,1])
      val_bench <- as.numeric(sortino_row %>% select(contains(benchmark_name)) %>% .[1,1])
      
      m_label <- paste0("m=1.", ifelse(m == "100", "00", substr(m, 2, 3)))
      df_summary_all <- rbind(df_summary_all, data.frame(
        Moneyness = m_label,
        Target_Sortino = val_target,
        Bench_Sortino = val_bench,
        Ratio = val_target / val_bench
      ))
    }
  }
  
  # 建立 flextable
  title_text <- paste0("Relative Sortino Ratio Efficiency: ", target_name, " vs. ", benchmark_name)
  
  ft <- flextable(df_summary_all) %>%
    add_header_lines(values = title_text) %>%
    set_header_labels(
      Moneyness = "Moneyness 價性",
      Target_Sortino = paste(target_name, "索丁諾比率"),
      Bench_Sortino = paste(benchmark_name, "索丁諾比率"),
      Ratio = paste0("比率 (", target_name, "/", benchmark_name, ")")
    ) %>%
    colformat_double(digits = 3) %>%
    border_remove() %>%
    # 設定實黑線
    hline_top(border = thick_border, part = "header") %>%
    hline(i = 1, border = std_border, part = "header") %>%
    hline_bottom(border = std_border, part = "header") %>%
    hline_bottom(border = thick_border, part = "body") %>%
    font(fontname = "Times New Roman", part = "all") %>%
    align(align = "center", part = "all") %>%
    bold(i = 1, part = "header") %>%
    autofit()
  
  # 寫入 Word
  doc <- read_docx() %>% body_add_flextable(value = ft)
  export_path <- file.path(base_path, "pictures", output_filename)
  print(doc, target = export_path)
  
  message(paste("✅ 已生成：", output_filename))
}

# 5. 執行生成
# (1) CBXM vs. IBXM
generate_comparison_report("CBXM", "IBXM", "CBXM_vs_IBXM_Sortino_Analysis.docx")

# (2) CBXM vs. SBXM
generate_comparison_report("CBXM", "SBXM", "CBXM_vs_SBXM_Sortino_Analysis.docx")