library(ggplot2)

# 參數
S0 <- 47000
K  <- 47000
C  <- 200

# 範圍與資料
S_T <- seq(46000, 48000, by = 50)
stock     <- S_T - S0
buy_write <- pmin(S_T - S0, K - S0) + C
df <- data.frame(S_T, stock, buy_write)

# 畫面範圍（原點正中心）
x_lim <- c(46000, 48000)
y_lim <- c(-1000, 1000)

# 軸線刻度與數字
x_major <- seq(46000, 48000, by = 200)
y_major <- seq(-1000, 1000, by = 200)
tick_len_x <- 35
tick_len_y <- 70
lab_off_x  <- -120   # X 軸數字往下移更多，避免貼到軸線
lab_off_y  <-  160   # Y 軸數字往右移更多

ticks_x <- data.frame(
  x = x_major, xend = x_major,
  y = 0 - tick_len_x/2, yend = 0 + tick_len_x/2,
  lab = x_major
)
ticks_y <- data.frame(
  x = S0 - tick_len_y/2, xend = S0 + tick_len_y/2,
  y = y_major, yend = y_major,
  lab = y_major
)
ticks_y$show_lab <- ticks_y$lab != 0
ticks_x$show_lab <- TRUE

p <- ggplot(df, aes(x = S_T)) +
  # 背景方格
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = 0.6),
    panel.grid.minor = element_line(color = "grey93", linewidth = 0.3),
    panel.background = element_rect(fill = "grey98", color = NA),
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    axis.line  = element_blank(),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, size = 16),
    # 左右上下邊界加大，避免數字被裁切
    plot.margin = margin(30, 40, 40, 60)   # top, right, bottom, left
  ) +
  labs(title = "Buy-write 策略報酬示意圖", color = "") +
  # 報酬線
  geom_line(aes(y = stock,     color = "美國道瓊指數Dow Jones"), linewidth = 1.1) +
  geom_line(aes(y = buy_write, color = "Buy-write 策略"), linewidth = 1.3, linetype = "dashed") +
  scale_color_manual(values = c("Buy-write 策略" = "darkgreen", "美國道瓊指數Dow Jones" = "red")) +
  # 中心軸線
  geom_hline(yintercept = 0,   linewidth = 0.8, color = "black") +
  geom_vline(xintercept = S0,  linewidth = 0.8, color = "black") +
  # 軸線刻度
  geom_segment(data = ticks_x, aes(x = x, xend = xend, y = y, yend = yend),
               linewidth = 0.6, inherit.aes = FALSE, color = "black") +
  geom_segment(data = ticks_y, aes(x = x, xend = xend, y = y, yend = yend),
               linewidth = 0.6, inherit.aes = FALSE, color = "black") +
  # 刻度數字
  geom_text(data = subset(ticks_x, show_lab),
            aes(x = x, y = lab_off_x, label = lab),
            size = 3.8, vjust = 1, inherit.aes = FALSE) +
  geom_text(data = subset(ticks_y, show_lab),
            aes(x = S0 + lab_off_y, y = y, label = lab),
            size = 3.8, hjust = 0, inherit.aes = FALSE) +
  # 固定比例、關閉裁切，長方形視圖
  coord_fixed(ratio = 0.6, xlim = x_lim, ylim = y_lim, expand = FALSE, clip = "off")

print(p)
ggsave("buy_write_center_ticks_wide.png", p, width = 9, height = 6, dpi = 300)
