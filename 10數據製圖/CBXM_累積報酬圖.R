# ==============================================================================
# Count Active MABXM Stocks (Bull Trend = 1) + DJI constituent filter
# ==============================================================================

library(readxl)
library(openxlsx)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(scales)  # 為了漂亮的日期刻度

# 1. 參數設定區 (讓你自由調整) ==========================================
output_width  <- 10      # 圖片寬度 (數值越大畫布越寬)
output_height <- 6       # 圖片高度
year_interval <- 3       # X 軸年份間隔 (目前設定為 4 年)

path_signal <- "C:/Users/User/Desktop/Thesis_project/Output_M100/全期間/Output_MABXM/MABXM_Signals_DailyMA.xlsx"
path_dji    <- "C:/Users/User/Desktop/Thesis_project/data_raw/DJI成分股名單.xls"
start_date  <- as.Date("2004-07-16")

# 2. 讀取訊號檔 ========================================================
signal_df <- read.xlsx(path_signal) %>%
  mutate(
    Trade_Date = as.Date(as.numeric(Trade_Date), origin = "1899-12-30"),
    Ticker = toupper(trimws(Ticker))
  )

# 篩選 Is_Bull_Trend = 1 (有執行策略)
signal_active <- signal_df %>%
  filter(Is_Bull_Trend == 1,
         Trade_Date >= start_date)

# 3. 讀取 DJI 成分股資料 (建立動態身分清單) =============================
dji_raw <- read_excel(path_dji)
equity_cols <- names(dji_raw)[grepl("%", names(dji_raw))]
month_map <- c("Jan"="01","Feb"="02","Mar"="03","Apr"="04","May"="05","Jun"="06",
               "Jul"="07","Aug"="08","Sep"="09","Oct"="10","Nov"="11","Dec"="12")

membership_list <- list()
for(i in 1:nrow(dji_raw)) {
  ticker_i <- toupper(trimws(as.character(dji_raw$ticker[i])))
  for(col in equity_cols) {
    val_num <- suppressWarnings(as.numeric(dji_raw[[col]][i]))
    if(!is.na(val_num)) {
      clean_col <- gsub(" % Of Equity", "", col); clean_col <- gsub("\\.", "-", clean_col)
      parts <- strsplit(clean_col, "-")[[1]]
      if(length(parts) >= 3 && parts[1] %in% names(month_map)) {
        date_str <- paste0(parts[3], "-", month_map[parts[1]], "-", parts[2])
        membership_list[[length(membership_list)+1]] <- data.frame(
          Ticker = ticker_i, Report_Date = as.Date(date_str), Is_Member = val_num > 0
        )
      }
    }
  }
}

membership_df <- bind_rows(membership_list) %>% arrange(Ticker, Report_Date)

# 4. 判斷是否為成分股並計算個股數 ======================================
get_member_status <- function(ticker, trade_date) {
  tmp <- membership_df %>% filter(Ticker == ticker, Report_Date <= trade_date) %>% arrange(Report_Date)
  if(nrow(tmp) == 0) return(FALSE)
  tail(tmp$Is_Member, 1) # 取離交易日最近的報告狀態
}

signal_active$Is_DJI_Member <- mapply(get_member_status, signal_active$Ticker, signal_active$Trade_Date)

daily_count <- signal_active %>%
  filter(Is_DJI_Member == TRUE) %>%
  group_by(Trade_Date) %>%
  summarise(Active_Stock_Count = n_distinct(Ticker)) %>%
  ungroup()

# 5. 畫圖 (黑色大外框、紅色點、黑色座標軸) =============================
p <- ggplot(daily_count, aes(x = Trade_Date, y = Active_Stock_Count)) +
  geom_line(color = "gray20", size = 1) +
  geom_point(color = "#C00000", size = 1.5) +
  # 設定年份間隔[cite: 1]
  scale_x_date(
    date_breaks = paste0(year_interval, " years"), 
    date_labels = "%Y",
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(
    title = "Number of Stocks Executing Strategy",
    x = "Date",
    y = "Number of Stocks"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    # 黑色大外框
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.2),
    # 座標軸線與背景設定
    axis.line = element_line(colour = "black", linewidth = 0.8),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA)
  )

print(p)

# 6. 存圖與輸出資料 ===================================================
ggsave(
  filename = "C:/Users/User/Desktop/Thesis_project/pictures/MBXM_COUNT.png",
  plot = p,
  width = output_width,
  height = output_height,
  dpi = 300
)



cat("完成：年份間隔與畫布尺寸已更新。\n")