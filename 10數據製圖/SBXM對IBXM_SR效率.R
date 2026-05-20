# ==========================================
# Relative Sortino Ratio Efficiency 分析 (表格標題與線條對齊版)
# ==========================================

# 1. 載入必要套件
if (!require("readxl")) install.packages("readxl")
if (!require("dplyr")) install.packages("dplyr")
if (!require("officer")) install.packages("officer")
if (!require("flextable")) install.packages("flextable")

library(readxl)
library(dplyr)
library(officer)
library(flextable)

# 2. 設定基礎路徑
base_path <- "C:/Users/User/Desktop/Thesis_project"
m_levels <- c("100", "101", "102", "103", "104", "105")
df_summary_all <- data.frame()

# 3. 循環讀取數據並計算比率
for (m in m_levels) {
  file_path <- file.path(base_path, paste0("Output_M", m), "全期間", paste0("Final_Report_M", m, ".xlsx"))
  
  if (file.exists(file_path)) {
    df_perf <- read_excel(file_path, sheet = "Performance_Summary")
    sortino_row <- df_perf %>% filter(grepl("Sortino", .[[1]]))
    
    ibxm_val <- as.numeric(sortino_row %>% select(contains("IBXM")) %>% .[1,1])
    sbxm_val <- as.numeric(sortino_row %>% select(contains("SBXM")) %>% .[1,1])
    
    m_label <- paste0("m=1.", ifelse(m == "100", "00", substr(m, 2, 3)))
    df_summary_all <- rbind(df_summary_all, data.frame(
      Moneyness = m_label,
      IBXM_Sortino = ibxm_val,
      SBXM_Sortino = sbxm_val,
      Ratio = sbxm_val / ibxm_val
    ))
  }
}

# 4. 建立 flextable 並精確控制實黑線
# 定義線條樣式
std_border <- fp_border(color = "black", width = 1.5)
thick_border <- fp_border(color = "black", width = 2.5)

ft <- flextable(df_summary_all) %>%
  # 將標題加入表格最上方，確保線條長度與表格一致
  add_header_lines(values = "Relative Sortino Ratio Efficiency: SBXM vs IBXM") %>%
  set_header_labels(
    Moneyness = "Moneyness 價性",
    IBXM_Sortino = "IBXM 索丁諾比率",
    SBXM_Sortino = "SBXM 索丁諾比率",
    Ratio = "比率 (SBXM/IBXM)"
  ) %>%
  colformat_double(digits = 3) %>%
  # 移除預設主題，手動設定線條
  border_remove() %>%
  # 1. 最上方實黑線 (粗)
  hline_top(border = thick_border, part = "header") %>%
  # 2. 標題與欄位名稱之間的隔線
  hline(i = 1, border = std_border, part = "header") %>%
  # 3. 欄位名稱與數據之間的隔線
  hline_bottom(border = std_border, part = "header") %>%
  # 4. 最下方底線
  hline_bottom(border = thick_border, part = "body") %>%
  # 字體與對齊設定
  font(fontname = "Times New Roman", part = "all") %>%
  align(align = "center", part = "all") %>%
  # 標題列文字加粗
  bold(i = 1, part = "header") %>%
  autofit()

# 5. 生成 Word 文件 (僅包含表格)
doc <- read_docx() %>%
  body_add_flextable(value = ft)

# 6. 匯出檔案
export_file <- "C:/Users/User/Desktop/Thesis_project/pictures/Sortino_Ratio_Analysis.docx"
print(doc, target = export_file)

message(paste("✅ 修正完成！實黑線已對齊，標題已併入表格：", export_file))