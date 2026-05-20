# 1. 載入必要套件
if (!require("readxl")) install.packages("readxl")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("flextable")) install.packages("flextable")
if (!require("officer")) install.packages("officer")

library(readxl)
library(tidyverse)
library(flextable)
library(officer)

# 2. 設定基礎路徑與參數
# 注意：這裡 base_path 指向 M100 資料夾
base_path <- "C:/Users/User/Desktop/Thesis_project/Output_M100"
periods <- c("第一期", "第二期", "第三期", "第四期")
strategies <- c("IBXM", "SBXM", "CBXM", "MBXM", "DBXM")

# 新增：讀取全期間的 VIX 訊號作為母表 (從您指定的 AXP 路徑)
vix_signal_path <- file.path(base_path, "全期間", "Output_CBXM", "CBXM_AXP.xlsx")
if (file.exists(vix_signal_path)) {
  vix_master <- read_excel(vix_signal_path, sheet = "Trade_Log") %>%
    select(Trading_Date, VIX_Signal) %>%
    mutate(Trading_Date = as.Date(Trading_Date))
} else {
  stop(paste("找不到 VIX 訊號檔案：", vix_signal_path))
}

# 建立清單存放四期的表格物件
final_ft_list <- list()

# 3. 開始批次處理各期資料
for (p_idx in 1:length(periods)) {
  p_name <- periods[p_idx]
  # 構建檔案路徑 (Final_Report1.xlsx, Final_Report2.xlsx ...)
  file_path <- file.path(base_path, p_name, paste0("Final_Report", p_idx, ".xlsx"))
  
  if (!file.exists(file_path)) {
    message(paste("跳過：找不到檔案", file_path))
    next
  }
  
  # 讀取 Excel 數據
  df_sum <- read_excel(file_path, sheet = "Performance_Summary")
  df_ret <- read_excel(file_path, sheet = "Monthly_Returns") %>%
    mutate(Date = as.Date(Date))
  
  # 將該期的月報酬與 VIX 訊號進行日期對應
  df_ret_merged <- df_ret %>%
    left_join(vix_master, by = c("Date" = "Trading_Date"))
  
  period_rows <- list()
  
  # -------------------------------------------------------------------------
  # A. 提取 DJITR (大盤基準)
  # -------------------------------------------------------------------------
  dji_row <- data.frame(
    策略 = "DJITR (基準)",
    平均報酬 = as.numeric(df_sum[df_sum$Metric == "Annualized Return", "DJITR"]) * 100,
    標準差 = as.numeric(df_sum[df_sum$Metric == "Annualized Std Dev", "DJITR"]) * 100,
    夏普比率 = as.numeric(df_sum[grep("Sharpe", df_sum$Metric), "DJITR"]),
    索丁諾比率 = as.numeric(df_sum[df_sum$Metric == "Sortino Ratio", "DJITR"]),
    最大回撤 = as.numeric(df_sum[df_sum$Metric == "Max Drawdown", "DJITR"]) * 100,
    資訊比率 = NA,
    優於大盤 = NA
  )
  period_rows[[1]] <- dji_row
  
  # -------------------------------------------------------------------------
  # B. 提取 DIA (ETF) - 採全期間勝率
  # -------------------------------------------------------------------------
  dia_outperform <- mean((df_ret[["DIA(ETF)"]] - df_ret[["DJITR"]]) > 1e-7, na.rm = TRUE) * 100
  
  dia_row <- data.frame(
    策略 = "DIA (ETF)",
    平均報酬 = as.numeric(df_sum[df_sum$Metric == "Annualized Return", "DIA(ETF)"]) * 100,
    標準差 = as.numeric(df_sum[df_sum$Metric == "Annualized Std Dev", "DIA(ETF)"]) * 100,
    夏普比率 = as.numeric(df_sum[grep("Sharpe", df_sum$Metric), "DIA(ETF)"]),
    索丁諾比率 = as.numeric(df_sum[df_sum$Metric == "Sortino Ratio", "DIA(ETF)"]),
    最大回撤 = as.numeric(df_sum[df_sum$Metric == "Max Drawdown", "DIA(ETF)"]) * 100,
    資訊比率 = as.numeric(df_sum[df_sum$Metric == "Information Ratio", "DIA(ETF)"]),
    優於大盤 = dia_outperform
  )
  period_rows[[2]] <- dia_row
  
  # -------------------------------------------------------------------------
  # C. 提取各策略 (M=1)
  # -------------------------------------------------------------------------
  for (strat in strategies) {
    strat_col <- paste0(strat, "(M=1)")
    
    # 優於大盤勝率計算邏輯分支
    if (strat %in% c("CBXM", "DBXM")) {
      # 擇時式勝率：僅計算 VIX_Signal == 1 的月份
      valid_subset <- df_ret_merged %>% filter(VIX_Signal == 1)
      if (nrow(valid_subset) > 0) {
        outperform_prob <- mean((valid_subset[[strat_col]] - valid_subset[["DJITR"]]) > 1e-7, na.rm = TRUE) * 100
      } else {
        outperform_prob <- NA # 若該子樣本期間完全沒有觸發 VIX，則設為 NA
      }
    } else {
      # IBXM, SBXM, MBXM 採全期間比較
      outperform_prob <- mean((df_ret[[strat_col]] - df_ret[["DJITR"]]) > 1e-7, na.rm = TRUE) * 100
    }
    
    new_row <- data.frame(
      策略 = strat_col,
      平均報酬 = as.numeric(df_sum[df_sum$Metric == "Annualized Return", strat_col]) * 100,
      標準差 = as.numeric(df_sum[df_sum$Metric == "Annualized Std Dev", strat_col]) * 100,
      夏普比率 = as.numeric(df_sum[grep("Sharpe", df_sum$Metric), strat_col]),
      索丁諾比率 = as.numeric(df_sum[df_sum$Metric == "Sortino Ratio", strat_col]),
      最大回撤 = as.numeric(df_sum[df_sum$Metric == "Max Drawdown", strat_col]) * 100,
      資訊比率 = as.numeric(df_sum[df_sum$Metric == "Information Ratio", strat_col]),
      優於大盤 = outperform_prob
    )
    period_rows[[length(period_rows) + 1]] <- new_row
  }
  
  # 合併該期所有列
  final_period_df <- do.call(rbind, period_rows)
  
  # -------------------------------------------------------------------------
  # D. 製作三線表格式
  # -------------------------------------------------------------------------
  ft <- flextable(final_period_df) %>%
    add_header_lines(values = paste0("表: ", p_name, "子樣本績效比較 (M=1)")) %>%
    font(fontname = "Times New Roman", part = "all") %>%
    fontsize(size = 10, part = "all") %>%
    colformat_double(j = c("平均報酬", "標準差", "最大回撤", "優於大盤"), digits = 2, suffix = "%") %>%
    colformat_double(j = c("夏普比率", "索丁諾比率", "資訊比率"), digits = 3, na_str = "-") %>%
    bold(part = "header") %>%
    border_remove() %>%
    hline_top(part = "header", border = fp_border(width = 1.5)) %>% 
    hline(i = 1, part = "header", border = fp_border(width = 0.8)) %>%
    hline(i = 2, part = "header", border = fp_border(width = 0.8)) %>%
    hline_bottom(part = "body", border = fp_border(width = 1.5)) %>%
    hline(i = 2, part = "body", border = fp_border(width = 0.8)) %>%
    align(align = "center", part = "all") %>%
    align(j = 1, align = "left", part = "all") %>%
    align(i = 1, align = "left", part = "header") %>%
    autofit()
  
  final_ft_list[[p_name]] <- ft
}

# -------------------------------------------------------------------------
# 4. 輸出至 Word 檔案
# -------------------------------------------------------------------------
output_doc <- read_docx()
for (p_name in periods) {
  if (!is.null(final_ft_list[[p_name]])) {
    output_doc <- output_doc %>% 
      body_add_flextable(value = final_ft_list[[p_name]]) %>%
      body_add_break() 
  }
}

target_path <- "C:/Users/User/Desktop/Thesis_project/pictures/各期績效.docx"
print(output_doc, target = target_path)

message(paste("完成！檔案已儲存至：", target_path))