# ==========================================
# 策略績效散布圖 - CBXM, MBXM, DBXM 標籤自訂控制與 CML 版
# ==========================================

library(readxl)
library(tidyverse)

# 1. 設定基礎路徑與參數
base_path <- "C:/Users/User/Desktop/Thesis_project"
m_levels <- c("100", "101", "102", "103", "104", "105")
target_strategies <- c("CBXM", "MBXM", "DBXM") 

# =========================================================
# 【標籤座標控制表】: 在這裡自由調整每個點的推移距離！
#  n_x: 左右推移 (正值往右，負值往左)
#  n_y: 上下推移 (正值往上，負值往下)
#  h_adj: 文字對齊 (0=靠左, 0.5=置中, 1=靠右)
# =========================================================
label_controls <- list(
  "CBXM" = list(
    "100" = list(n_x = 0, n_y = -0.04, h_adj = 0.5),
    "101" = list(n_x = 0, n_y = -0.08, h_adj = 0.5),
    "102" = list(n_x = 0, n_y = 0.05, h_adj = 0.5),
    "103" = list(n_x = 0, n_y = 0.05, h_adj = 0.5),
    "104" = list(n_x = 0, n_y = 0.05, h_adj = 0.5),
    "105" = list(n_x = 0, n_y = 0.05, h_adj = 0.5)
  ),
  "MBXM" = list(
    "100" = list(n_x = -0.1, n_y = 0.025, h_adj = 0.5),
    "101" = list(n_x = -0.12, n_y = -0.025, h_adj = 0.5), 
    "102" = list(n_x = 0.12, n_y = 0, h_adj = 0.5),
    "103" = list(n_x = 0.12, n_y = 0, h_adj = 0.5),
    "104" = list(n_x = 0, n_y = 0.05, h_adj = 0.5),
    "105" = list(n_x = 0, n_y = 0.05, h_adj = 0.5)
  ),
  "DBXM" = list(
    "100" = list(n_x = 0, n_y = -0.04, h_adj = 0.5), 
    "101" = list(n_x = 0, n_y =  -0.08, h_adj = 0.5),
    "102" = list(n_x = 0,    n_y = 0.05, h_adj = 0.5),
    "103" = list(n_x = 0,    n_y = 0.05, h_adj = 0.5),
    "104" = list(n_x = 0,    n_y = 0.05, h_adj = 0.5),
    "105" = list(n_x = 0,    n_y = 0.05, h_adj = 0.5)
  )
)
# =========================================================

all_data <- list()

# 2. 提取各策略資料
for (m in m_levels) {
  file_path <- file.path(base_path, paste0("Output_M", m), "全期間", paste0("Final_Report_M", m, ".xlsx"))
  if(!file.exists(file_path)) next
  
  df_sum <- read_excel(file_path, sheet = "Performance_Summary")
  m_val <- as.numeric(m) / 100
  
  for (strat in target_strategies) {
    strat_col <- ifelse(m == "100", paste0(strat, "(M=1)"), paste0(strat, "(M=", m_val, ")"))
    if (!(strat_col %in% names(df_sum))) {
      strat_col <- names(df_sum)[grepl(paste0("^", strat, "\\("), names(df_sum))][1]
    }
    
    if (!is.na(strat_col) && strat_col %in% names(df_sum)) {
      
      # 根據控制表抓取對應設定
      ctrl <- label_controls[[strat]][[m]]
      
      all_data[[length(all_data) + 1]] <- data.frame(
        Strategy = strat,
        Moneyness = paste0("m=", m_val),
        Return = as.numeric(df_sum[df_sum$Metric == "Annualized Return", strat_col]) * 100,
        Risk = as.numeric(df_sum[df_sum$Metric == "Annualized Std Dev", strat_col]) * 100,
        n_y = ctrl$n_y, 
        n_x = ctrl$n_x, 
        h_adj = ctrl$h_adj
      )
    }
  }
}
plot_df <- do.call(rbind, all_data)

# 設定因子順序以確保圖例與繪圖順序為 CBXM -> MBXM -> DBXM
plot_df$Strategy <- factor(plot_df$Strategy, levels = c("CBXM", "MBXM", "DBXM"))

# 3. 讀取基準點 (DIA 與 Rf) 
portfolio_path <- "C:/Users/User/Desktop/Thesis_project/Output_M100/全期間/Output_SBXM/SBXM_Portfolio_M100.xlsx"

if (file.exists(portfolio_path)) {
  df_log <- read_excel(portfolio_path, sheet = "Portfolio_Log")
  df_log$days <- as.numeric(difftime(df_log$Expiry_Date_Ref, df_log$Date_For_Port, units = "days"))
  
  rf_mean <- sum(df_log$Avg_Rf * df_log$days, na.rm = TRUE) / sum(df_log$days, na.rm = TRUE)
  rf_sd <- sqrt(sum(df_log$days * (df_log$Avg_Rf - rf_mean)^2, na.rm = TRUE) / sum(df_log$days, na.rm = TRUE))
  
  df_perf <- read_excel(portfolio_path, sheet = "Performance")
  dji_ret <- as.numeric(df_perf[df_perf$Metric == "Annualized Return", "DIA(ETF)"]) * 100
  dji_sd  <- as.numeric(df_perf[df_perf$Metric == "Annualized Std Dev", "DIA(ETF)"]) * 100
  
  benchmark_df <- data.frame(
    Type = c("Risk-Free Rate", "DIA ETF"),
    Return = c(rf_mean, dji_ret),
    Risk = c(rf_sd, dji_sd)
  )
}

# 4. 繪圖
p <- ggplot() +
  geom_segment(aes(x = benchmark_df$Risk[1], y = benchmark_df$Return[1], 
                   xend = benchmark_df$Risk[2], yend = benchmark_df$Return[2]), 
               linetype = "longdash", color = "grey50", size = 0.6) +
  geom_path(data = plot_df, aes(x = Risk, y = Return, color = Strategy, group = Strategy), 
            alpha = 0.3, linetype = "dashed") +
  geom_point(data = plot_df, aes(x = Risk, y = Return, color = Strategy, shape = Strategy), size = 5) +
  
  geom_point(data = benchmark_df, aes(x = Risk, y = Return), 
             shape = 18, size = 6, color = "grey40") +
  
  geom_text(data = benchmark_df, aes(x = Risk, y = Return, label = Type), 
            vjust = -1.8, family = "serif", fontface = "italic", size = 4, color = "grey40") +
  
  geom_text(data = plot_df, aes(x = Risk, y = Return, label = Moneyness, hjust = h_adj, color = Strategy), 
            nudge_y = plot_df$n_y, nudge_x = plot_df$n_x,
            size = 3.5, fontface = "bold", family = "serif", show.legend = FALSE) +
  
  scale_color_manual(values = c("CBXM" = "#FF8C00", "MBXM" = "#2E8B57", "DBXM" = "#800080")) +
  # 設定指定圖形：CBXM=17, MBXM=16, DBXM=15
  scale_shape_manual(values = c("CBXM" = 17, "MBXM" = 16, "DBXM" = 15)) +
  
  # 視角放大，這裡先預設與上一張圖相近的範圍，你可視跑出來的圖再做微調
  coord_cartesian(xlim = c(14.5, 18), ylim = c(8, 9.5)) + 
  
  theme_bw() + 
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = "bottom"
  ) +
  labs(
    title = "Risk-Return Scatter Plot : Strategic Comparison",
    x = "Annualized Standard Deviation (%)",
    y = "Annualized Return (%)"
  )

# 5. 輸出
export_path <- file.path(base_path, "pictures", "Risk_Return_Scatter_C_M_D.png")
if (!dir.exists(dirname(export_path))) dir.create(dirname(export_path), recursive = TRUE)
ggsave(export_path, plot = p, width = 11, height = 8, dpi = 300)

message("✅ 圖表已產出：CBXM, MBXM, DBXM 標籤控制表建立，CML 與指定圖形已設定完成！")