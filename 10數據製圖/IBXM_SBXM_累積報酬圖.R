# ==========================================
# IBXM vs SBXM 策略績效比較圖 (自訂粗細版)
# ==========================================
Sys.setlocale("LC_TIME", "C")
library(readxl)
library(dplyr)
library(xts)
library(PerformanceAnalytics)

# 1. 設定檔案路徑
paths <- list(
  m100 = "C:/Users/User/Desktop/Thesis_project/Output_M100/全期間/Final_Report_M100.xlsx",
  m102 = "C:/Users/User/Desktop/Thesis_project/Output_M102/全期間/Final_Report_M102.xlsx",
  m104 = "C:/Users/User/Desktop/Thesis_project/Output_M104/全期間/Final_Report_M104.xlsx"
)

# 2. 讀取資料函數
load_strat_comparison <- function(p, m_val) {
  df <- read_excel(p, sheet = "Monthly_Returns")
  date_col <- names(df)[1]
  ibxm_col <- names(df)[grepl("IBXM", names(df))][1]
  sbxm_col <- names(df)[grepl("SBXM", names(df))][1]
  df_sub <- df %>%
    select(all_of(c(date_col, ibxm_col, sbxm_col))) %>%
    mutate(across(-1, ~as.numeric(as.character(.)))) %>%
    rename(
      Date = !!date_col,
      !!paste0("IBXM (m=", m_val, ")") := !!ibxm_col,
      !!paste0("SBXM (m=", m_val, ")") := !!sbxm_col
    ) %>%
    mutate(Date = as.Date(Date))
  return(df_sub)
}

# 3. 資料合併
df_m100 <- load_strat_comparison(paths$m100, "1.00")
df_m102 <- load_strat_comparison(paths$m102, "1.02")
df_m104 <- load_strat_comparison(paths$m104, "1.04")

df_final <- df_m100 %>%
  full_join(df_m102, by = "Date") %>%
  full_join(df_m104, by = "Date") %>%
  arrange(Date) %>%
  select(
    Date,
    contains("IBXM (m=1.00)"), contains("IBXM (m=1.02)"), contains("IBXM (m=1.04)"),
    contains("SBXM (m=1.00)"), contains("SBXM (m=1.02)"), contains("SBXM (m=1.04)")
  )

ts_ret <- xts(df_final[,-1], order.by = df_final$Date)
storage.mode(ts_ret) <- "double"
ts_ret[is.na(ts_ret)] <- 0

# 4. 計算財富指數
wealth_index <- cumprod(1 + ts_ret)
date_idx     <- index(wealth_index)
wi_mat       <- coredata(wealth_index)

# 5. 顏色與線條
my_colors <- c("#ADD8E6", "#6495ED", "#00008B", "#F08080", "red", "darkred")
my_lty    <- c(6, 6, 6, 1, 1, 1)

# 【新增】自訂每條線的粗細 (順序對應: IBXM m=1.00, 1.02, 1.04, SBXM m=1.00, 1.02, 1.04)
# 你可以在這裡更改數值
my_lwd    <- c(1.5, 1.5, 1.5, 1.5, 1.8, 2.1) 

# 6. 右上角時間標籤（對齊第二份圖風格）
date_start <- format(min(date_idx), "%Y-%m-%d")
date_end   <- format(max(date_idx), "%Y-%m-%d")
date_label <- paste(date_start, "/", date_end)

# 7. x 軸刻度：從資料起始月份往後每 2 年
#    起點用資料第一個日期當月（Aug 2004），之後每 2 年
x_start <- as.Date(format(min(date_idx), "%Y-%m-01"))  # 2004-08-01
year_seq <- seq(from = x_start, by = "2 years", length.out = 20)
year_seq <- year_seq[year_seq <= max(date_idx)]         # 裁掉超出範圍的

# 8. 匯出設定
export_dir  <- "C:/Users/User/Desktop/Thesis_project/pictures"
if (!dir.exists(export_dir)) dir.create(export_dir, recursive = TRUE)
export_file <- file.path(export_dir, "IBXM_SBXM_Comparison.png")

png(filename = export_file, width = 1200, height = 800, res = 150)

par(
  mar      = c(4, 3, 4, 6),   # 右邊留空間放日期標籤
  mgp      = c(2, 0.5, 0),
  tcl      = -0.3,
  cex.axis = 0.85,
  cex.lab  = 0.9
)

plot(
  x    = date_idx,
  y    = wi_mat[, 1],
  type = "n",
  ylim = range(wi_mat, na.rm = TRUE),
  xaxt = "n",
  ylab = "",
  xlab = "",
  bty  = "l",
  main = ""   # title 手動加，這樣可以控制日期在右上角
)

# 主標題（左對齊）+ 右上角日期標籤
mtext("Cumulative Returns: IBXM vs SBXM at different Moneyness",
      side = 3, adj = 0, cex = 1.0, line = 2, font = 2)
mtext(date_label,
      side = 3, adj = 1, cex = 0.8, line = 2, col = "grey40")

# x 軸：Aug 2004 起，每 2 年一格
axis(1,
     at     = as.numeric(year_seq),
     labels = format(year_seq, "%b %Y"),
     las    = 1)

# 格線（與刻度完全對齊）
abline(h = axTicks(2),            col = "grey88", lty = 1, lwd = 0.8)
abline(v = as.numeric(year_seq),  col = "grey88", lty = 1, lwd = 0.8)

# 畫線條
for (i in seq_len(ncol(wi_mat))) {
  lines(
    x   = date_idx,
    y   = wi_mat[, i],
    col = my_colors[i],
    lty = my_lty[i],
    lwd = my_lwd[i]  # 套用自訂的粗細陣列
  )
}

# 圖例（無外框）
legend(
  "topleft",
  legend = colnames(wealth_index),
  col    = my_colors,
  lty    = my_lty,
  lwd    = my_lwd,   # 圖例也會同步顯示你設定的粗細
  cex    = 0.78,
  bty    = "n"
)

dev.off()
message(paste("✅ 完成！已匯出：", export_file))