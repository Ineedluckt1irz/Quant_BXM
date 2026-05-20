# ==========================================
# MBXM vs DIA 策略績效比較圖 (綠色調梯度版 - 自訂粗細)
# ==========================================
Sys.setlocale("LC_TIME", "C")
library(readxl)
library(dplyr)
library(xts)
library(PerformanceAnalytics)

# 1. 設定檔案路徑
paths_mbxm <- list(
  m100 = "C:/Users/User/Desktop/Thesis_project/Output_M100/全期間/Final_Report_M100.xlsx",
  m102 = "C:/Users/User/Desktop/Thesis_project/Output_M102/全期間/Final_Report_M102.xlsx",
  m104 = "C:/Users/User/Desktop/Thesis_project/Output_M104/全期間/Final_Report_M104.xlsx"
)

# 2. 讀取資料函數 (目標列改為搜尋 "MBXM")
load_final_ret <- function(p, m_val) {
  df <- read_excel(p, sheet = "Monthly_Returns")
  date_col <- names(df)[1] 
  target_col <- names(df)[grepl("MBXM", names(df))][1]
  
  df_sub <- df %>%
    select(all_of(c(date_col, target_col))) %>%
    mutate(across(-1, ~as.numeric(as.character(.)))) %>% 
    rename(Date = !!date_col, !!paste0("MBXM (m=", m_val, ")") := !!target_col) %>%
    mutate(Date = as.Date(Date))
  return(df_sub)
}

# 3. 讀取與合併資料
df_m100 <- load_final_ret(paths_mbxm$m100, "1.00")
df_m102 <- load_final_ret(paths_mbxm$m102, "1.02")
df_m104 <- load_final_ret(paths_mbxm$m104, "1.04")

# 讀取 DIA 資料 (基準)
df_dia_raw <- read_excel(paths_mbxm$m100, sheet = "Monthly_Returns")
dia_col <- names(df_dia_raw)[grepl("DIA", names(df_dia_raw))][1]
df_dia <- df_dia_raw %>%
  select(Date = 1, `DIA (ETF)` = !!dia_col) %>%
  mutate(Date = as.Date(Date), `DIA (ETF)` = as.numeric(as.character(`DIA (ETF)`)))

# 最終合併並排序
df_final <- df_dia %>%
  full_join(df_m100, by = "Date") %>%
  full_join(df_m102, by = "Date") %>%
  full_join(df_m104, by = "Date") %>%
  arrange(Date)

ts_ret <- xts(df_final[,-1], order.by = df_final$Date)
storage.mode(ts_ret) <- "double"
ts_ret[is.na(ts_ret)] <- 0

# 4. 計算財富指數 (Wealth Index)
wealth_index <- cumprod(1 + ts_ret)
date_idx     <- index(wealth_index)
wi_mat        <- coredata(wealth_index)

# 5. 顏色與線條設定
# 調整重點：MBXM 顏色設定為 darkgreen 及其深色梯度
my_colors <- c("#778899", "#81C784", "#43A047", "#1B5E20")
my_lty    <- c(6, 1, 1, 1) 

# 【新增】自訂每條線的粗細 (順序對應：DIA, m=1.00, m=1.02, m=1.04)
# 你可以在這裡更改數值，目前預設皆為 1.8
my_lwd    <- c(1.8, 1.8, 2.1, 2.6)

# 6. 右上角時間標籤
date_start <- format(min(date_idx), "%Y-%m-%d")
date_end   <- format(max(date_idx), "%Y-%m-%d")
date_label <- paste(date_start, "/", date_end)

# 7. X 軸刻度：每 2 年一次
x_start  <- as.Date(format(min(date_idx), "%Y-%m-01"))
year_seq <- seq(from = x_start, by = "2 years", length.out = 20)
year_seq <- year_seq[year_seq <= max(date_idx)]

# 8. 匯出設定與繪圖
export_dir  <- "C:/Users/User/Desktop/Thesis_project/pictures"
if (!dir.exists(export_dir)) dir.create(export_dir, recursive = TRUE)
export_file <- file.path(export_dir, "MBXM_Moneyness_Comparison.png")

png(filename = export_file, width = 1200, height = 800, res = 150)

# 設定邊距
par(
  mar      = c(4, 3, 4, 6),
  mgp      = c(2, 0.5, 0),
  tcl      = -0.3,
  cex.axis = 0.85,
  cex.lab  = 0.9
)

# 初始化畫布
plot(
  x    = date_idx,
  y    = wi_mat[, 1],
  type = "n",
  ylim = range(wi_mat, na.rm = TRUE),
  xaxt = "n",
  ylab = "",
  xlab = "",
  bty  = "l",
  main = ""
)

# 主標題與日期標籤
mtext("Cumulative Returns: DIA vs MBXM at different Moneyness",
      side = 3, adj = 0, cex = 1.0, line = 2, font = 2)
mtext(date_label,
      side = 3, adj = 1, cex = 0.8, line = 2, col = "grey40")

# 自定義 X 軸
axis(1,
     at      = as.numeric(year_seq),
     labels = format(year_seq, "%b %Y"),
     las    = 1)

# 加入背景格線
abline(h = axTicks(2),            col = "grey88", lty = 1, lwd = 0.8)
abline(v = as.numeric(year_seq),  col = "grey88", lty = 1, lwd = 0.8)

# 繪製各條曲線
for (i in seq_len(ncol(wi_mat))) {
  lines(
    x   = date_idx,
    y   = wi_mat[, i],
    col = my_colors[i],
    lty = my_lty[i],
    lwd = my_lwd[i]  # 套用自訂的粗細陣列
  )
}

# 圖例設定
legend(
  "topleft",
  legend = colnames(wealth_index),
  col    = my_colors,
  lty    = my_lty,
  lwd    = my_lwd,   # 圖例也會同步顯示你設定的粗細
  cex    = 0.8,
  bty    = "n"
)

dev.off()
message(paste("✅ 已完成綠色系 MBXM vs DIA 績效圖，匯出至：", export_file))