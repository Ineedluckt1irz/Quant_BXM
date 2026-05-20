# 1. 載入必要套件
library(readxl)
library(tidyverse)
library(flextable)
library(officer)

# 2. 設定基礎路徑與參數
base_path <- "C:/Users/User/Desktop/Thesis_project"
m_levels <- c("100", "101", "102", "103", "104", "105")
strategies <- c("IBXM", "SBXM", "CBXM", "MBXM", "DBXM")

all_rows <- list()

# -------------------------------------------------------------------------
# A. 提取基準 (DJITR 與 DIA ETF)
# -------------------------------------------------------------------------
m100_path <- file.path(base_path, "Output_M100/全期間/Final_Report_M100.xlsx")
summary_m100 <- read_excel(m100_path, sheet = "Performance_Summary")
returns_m100 <- read_excel(m100_path, sheet = "Monthly_Returns")

# 1. DJITR 數據
dji_row <- data.frame(
  策略 = "DJITR",
  平均報酬 = as.numeric(summary_m100[summary_m100$Metric == "Annualized Return", "DJITR"]) * 100,
  標準差 = as.numeric(summary_m100[summary_m100$Metric == "Annualized Std Dev", "DJITR"]) * 100,
  夏普比率 = as.numeric(summary_m100[grep("Sharpe", summary_m100$Metric), "DJITR"]),
  索丁諾比率 = as.numeric(summary_m100[summary_m100$Metric == "Sortino Ratio", "DJITR"]),
  最大回撤 = as.numeric(summary_m100[summary_m100$Metric == "Max Drawdown", "DJITR"]) * 100,
  資訊比率 = NA,
  優於大盤 = NA
)

# 2. DIA (ETF) 數據 (全期間勝率)
dia_outperform <- mean((returns_m100[["DIA(ETF)"]] - returns_m100[["DJITR"]]) > 1e-7, na.rm = TRUE) * 100

dia_row <- data.frame(
  策略 = "DIA (ETF)",
  平均報酬 = as.numeric(summary_m100[summary_m100$Metric == "Annualized Return", "DIA(ETF)"]) * 100,
  標準差 = as.numeric(summary_m100[summary_m100$Metric == "Annualized Std Dev", "DIA(ETF)"]) * 100,
  夏普比率 = as.numeric(summary_m100[grep("Sharpe", summary_m100$Metric), "DIA(ETF)"]),
  索丁諾比率 = as.numeric(summary_m100[summary_m100$Metric == "Sortino Ratio", "DIA(ETF)"]),
  最大回撤 = as.numeric(summary_m100[summary_m100$Metric == "Max Drawdown", "DIA(ETF)"]) * 100,
  資訊比率 = as.numeric(summary_m100[summary_m100$Metric == "Information Ratio", "DIA(ETF)"]),
  優於大盤 = dia_outperform
)

all_rows[[1]] <- dji_row
all_rows[[2]] <- dia_row

# -------------------------------------------------------------------------
# B. 處理五種策略 x 六種價性
# -------------------------------------------------------------------------
for (strat in strategies) {
  for (m in m_levels) {
    file_path <- file.path(base_path, paste0("Output_M", m), "全期間", paste0("Final_Report_M", m, ".xlsx"))
    
    df_sum <- read_excel(file_path, sheet = "Performance_Summary")
    df_ret <- read_excel(file_path, sheet = "Monthly_Returns")
    
    m_val <- as.numeric(m) / 100
    strat_col <- ifelse(m == "100", paste0(strat, "(M=1)"), paste0(strat, "(M=", m_val, ")"))
    
    if (!(strat_col %in% names(df_sum))) {
      strat_col <- names(df_sum)[grepl(paste0("^", strat, "\\("), names(df_sum))][1]
    }
    
    # --- 優於大盤邏輯處理：CBXM 與 DBXM 採擇時式勝率 ---
    if (strat %in% c("CBXM", "DBXM")) {
      # 讀取對應價性的 VIX 訊號 (從 CBXM_AXP 提取市場層級訊號)
      vix_path <- file.path(base_path, paste0("Output_M", m), "全期間", "Output_CBXM", "CBXM_AXP.xlsx")
      df_vix <- read_excel(vix_path, sheet = "Trade_Log")
      
      # 僅篩選 VIX 訊號觸發月份 (VIX_Signal == 1)
      valid_months <- df_vix$VIX_Signal == 1
      
      # 計算執行月份中的勝率
      outperform_prob <- mean((df_ret[[strat_col]][valid_months] - df_ret[["DJITR"]][valid_months]) > 1e-7, na.rm = TRUE) * 100
    } else {
      # IBXM, SBXM, MBXM 採全期間比較
      outperform_prob <- mean((df_ret[[strat_col]] - df_ret[["DJITR"]]) > 1e-7, na.rm = TRUE) * 100
    }
    
    new_row <- data.frame(
      策略 = paste0(strat, " (M=", m_val, ")"),
      平均報酬 = as.numeric(df_sum[df_sum$Metric == "Annualized Return", strat_col]) * 100,
      標準差 = as.numeric(df_sum[df_sum$Metric == "Annualized Std Dev", strat_col]) * 100,
      夏普比率 = as.numeric(df_sum[grep("Sharpe", df_sum$Metric), strat_col]),
      索丁諾比率 = as.numeric(df_sum[df_sum$Metric == "Sortino Ratio", strat_col]),
      最大回撤 = as.numeric(df_sum[df_sum$Metric == "Max Drawdown", strat_col]) * 100,
      資訊比率 = as.numeric(df_sum[df_sum$Metric == "Information Ratio", strat_col]),
      優於大盤 = outperform_prob
    )
    all_rows[[length(all_rows) + 1]] <- new_row
  }
}

final_table <- do.call(rbind, all_rows)

# -------------------------------------------------------------------------
# C. 製作最終三線表
# -------------------------------------------------------------------------
base_rows <- 2 
group_size <- length(m_levels) 
hline_indices <- base_rows + (0:4) * group_size

ft <- flextable(final_table) %>%
  add_header_lines(values = "表1:不同策略與DJI之績效比較") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  colformat_double(j = c("平均報酬", "標準差", "最大回撤", "優於大盤"), digits = 2, suffix = "%") %>%
  colformat_double(j = c("夏普比率", "索丁諾比率", "資訊比率"), digits = 3, na_str = "-") %>%
  bold(part = "header") %>%
  border_remove() %>%
  hline_top(part = "header", border = fp_border(width = 1.5)) %>% 
  hline_bottom(part = "body", border = fp_border(width = 1.5)) %>%
  hline(i = 1, part = "header", border = fp_border(width = 0.8)) %>%
  hline(i = 2, part = "header", border = fp_border(width = 0.8)) %>%
  hline(i = hline_indices, part = "body", border = fp_border(width = 0.8)) %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  align(i = 1, align = "left", part = "header") %>%
  autofit()

# 輸出
print(ft, preview = "docx")