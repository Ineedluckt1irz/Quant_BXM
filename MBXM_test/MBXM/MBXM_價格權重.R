# ==============================================================================
# 最終修正版：MBXM 投資組合整合 (完全對齊 SBXM 價格權重標準版)
# ==============================================================================

# 1. 套件載入 =====
library(readxl)
library(dplyr)
library(lubridate)
library(PerformanceAnalytics)
library(xts)
library(openxlsx)
library(tidyr)
library(stringr)
library(RSQLite)
library(DBI)

# ==============================================================================
# 2. 設定區
# ==============================================================================
current_path <- getwd()
path_db             <- "C:/Users/User/Desktop/Thesis_project/Data/SQLDB1.sqlite" 
path_input_folder   <- "C:/Users/User/Desktop/Thesis_project/Output_MBXM/"
path_dia_file       <- "C:/Users/User/Desktop/Thesis_project/data_raw/DIA.xls"
output_filename     <- "MBXM_Portfolio.xlsx"

Sys.setlocale("LC_TIME", "C") 

clean_excel_date <- function(x) {
  if (is.numeric(x)) as.Date(x, origin = "1899-12-30") else as.Date(x)
}

get_price_fn <- function(query_date, price_df) {
  if (is.null(price_df) || nrow(price_df) == 0) return(NA)
  subset <- price_df %>% filter(Date <= query_date)
  if (nrow(subset) == 0) return(NA)
  return(tail(subset$Close, 1))
}

# ==============================================================================
# 3. 讀取 SQL 與市場資料
# ==============================================================================
message("🔍 連線 SQL 資料庫...")
con <- dbConnect(RSQLite::SQLite(), path_db)

# 讀取 DJITR (從 SQL)
djitr_clean <- tryCatch({
  df <- dbReadTable(con, "dji_index")
  df %>% rename(Date = 1, Close = 2) %>%
    mutate(Date = as.Date(Date), Close = as.numeric(Close)) %>% arrange(Date)
}, error = function(e) { message("⚠️ SQL 大盤資料讀取失敗"); data.frame() })

# 讀取 DIA (從 Excel)
dia_clean <- if(file.exists(path_dia_file)) {
  read_excel(path_dia_file) %>% select(1, 2) %>% setNames(c("Date", "Close")) %>%
    mutate(Date = clean_excel_date(Date), Close = as.numeric(Close)) %>% arrange(Date)
} else { data.frame() }

# ==============================================================================
# 4. Step 1: 讀取 MBXM 個股回測結果
# ==============================================================================
message("🔍 Step 1: 讀取 MBXM 個股回測檔...")
file_list <- list.files(path_input_folder, pattern = "MBXM_.*\\.xlsx", full.names = TRUE)
file_list <- file_list[!grepl("Portfolio", basename(file_list))] 

stock_data <- data.frame()

for (f in file_list) {
  if (grepl("^~\\$", basename(f))) next 
  
  try({
    # 解析 Ticker
    ticker <- str_match(basename(f), "MBXM_(.*)\\.xlsx")[2]
    tmp <- readxl::read_excel(f, sheet = "Trade_Log")
    names(tmp) <- tolower(names(tmp))
    
    # 按照 SBXM 邏輯計算隱含價值，並保留 Is_Strong
    tmp <- tmp %>% mutate(
      Date_For_Port = clean_excel_date(trading_date),
      Date_Expiry   = clean_excel_date(expiry_date),
      Ticker = ticker,
      # 價值結算：收盤價 + 選擇權損益 + 股利
      Term_Value_MBXM = close_t1 + payoff + dividend,
      Implied_Cost_MBXM = Term_Value_MBXM / (1 + mbxm_return),
      Term_Value_BnH = close_t1 + dividend,
      Implied_Cost_BnH = Term_Value_BnH / (1 + bnh_return),
      Strong_Flag = is_strong
    ) %>% filter(!is.na(mbxm_return))
    
    stock_data <- rbind(stock_data, tmp)
  }, silent = FALSE)
}

if (nrow(stock_data) == 0) stop("❌ 錯誤：找不到個股資料，請檢查路徑。")

# ==============================================================================
# 5. Step 2: 成分股匹配 (精確對齊 SQL 30 隻名單)
# ==============================================================================
message("🔍 Step 2: 處理 SQL 成分股動態匹配...")
components_raw <- dbReadTable(con, "dji_constituents_raw")
dbDisconnect(con)

# 欄位校正與日期清洗 (對齊 SBXM 版強大正則)
t_col <- grep("ticker", names(components_raw), ignore.case = TRUE, value = TRUE)[1]
if(!is.na(t_col)) names(components_raw)[names(components_raw) == t_col] <- "Ticker"
date_cols <- grep("Equity", names(components_raw), ignore.case = TRUE, value = TRUE)
date_cols <- date_cols[date_cols != "Latest % Of Equity"]

components_db <- components_raw %>%
  select(Ticker, all_of(date_cols)) %>%
  pivot_longer(cols = -Ticker, names_to = "Date_String", values_to = "Weight") %>%
  filter(!is.na(Weight) & Weight > 0) %>%
  mutate(
    Clean_Date = str_replace_all(Date_String, "[^a-zA-Z0-9]", " "),
    Clean_Date = str_remove_all(Clean_Date, "(?i)of equity"),
    Comp_Date_Obj = as.Date(parse_date_time(Clean_Date, orders = c("mdy", "bdy", "dmy", "ymd")))
  ) %>%
  filter(!is.na(Comp_Date_Obj))

# 建立日期匹配表
unique_trade_dates <- sort(unique(stock_data$Date_For_Port))
component_dates    <- sort(unique(components_db$Comp_Date_Obj))

match_table <- data.frame(Trade_Date = unique_trade_dates)
match_table$Target_Comp_Date <- as.Date(sapply(match_table$Trade_Date, function(d) {
  past_dates <- component_dates[component_dates <= d]
  if(length(past_dates) > 0) max(past_dates) else min(component_dates)
}), origin = "1970-01-01")

# 合併投組：僅保留當時身分為 30 隻成分股的數據
portfolio_data <- stock_data %>%
  left_join(match_table, by = c("Date_For_Port" = "Trade_Date")) %>%
  inner_join(components_db, by = c("Ticker" = "Ticker", "Target_Comp_Date" = "Comp_Date_Obj"))

# ==============================================================================
# 6. Step 3: 計算投組績效 (價格權重聚合)
# ==============================================================================
message("🔄 Step 3: 計算投組績效 (Price-Weighted)...")

portfolio_ts <- portfolio_data %>%
  group_by(Date_For_Port) %>%
  summarise(
    Component_Count = n(),
    Strong_Stock_Count = sum(Strong_Flag, na.rm = TRUE),
    Expiry_Date_Ref = first(Date_Expiry),
    Cost_MBXM = sum(Implied_Cost_MBXM, na.rm = TRUE),
    Value_MBXM = sum(Term_Value_MBXM, na.rm = TRUE),
    Cost_BnH = sum(Implied_Cost_BnH, na.rm = TRUE),
    Value_BnH = sum(Term_Value_BnH, na.rm = TRUE),
    Avg_Rf = mean(riskfreerate_30d, na.rm = TRUE)
  ) %>%
  rowwise() %>%
  mutate(
    Portfolio_Return_MBXM = (Value_MBXM / Cost_MBXM) - 1,
    Portfolio_Return_BnH  = (Value_BnH / Cost_BnH) - 1,
    DIA_Return = (get_price_fn(Expiry_Date_Ref, dia_clean) / get_price_fn(Date_For_Port, dia_clean)) - 1,
    DJITR_Return = (get_price_fn(Expiry_Date_Ref, djitr_clean) / get_price_fn(Date_For_Port, djitr_clean)) - 1
  ) %>%
  ungroup() %>% filter(!is.na(Portfolio_Return_MBXM))

# ==============================================================================
# 7. Step 6: 輸出報表與繪圖 (含指標清洗與無限值修正)
# ==============================================================================
message("📊 Step 6: 產出最終績效指標與圖表...")

ts_cols  <- c("Portfolio_Return_MBXM", "Portfolio_Return_BnH", "DIA_Return", "DJITR_Return")
ts_names <- c("MBXM(M=1)", "BnH", "DIA(ETF)", "DJITR")

ts_ret <- xts(portfolio_ts[, ts_cols], order.by = portfolio_ts$Date_For_Port)
colnames(ts_ret) <- ts_names
ts_rf <- xts(portfolio_ts$Avg_Rf / 1200, order.by = portfolio_ts$Date_For_Port)

# 指標計算
tab_ann <- table.AnnualizedReturns(ts_ret, Rf = ts_rf, scale = 12)
max_dd  <- maxDrawdown(ts_ret)
sortino <- SortinoRatio(ts_ret, MAR = 0)
calmar  <- CalmarRatio(ts_ret, scale = 12)

# 相對績效 (Beta, Treynor, IR)
market_ret <- ts_ret[, "DJITR"]
n_cols <- ncol(ts_ret)
beta_vec <- numeric(n_cols); treynor_vec <- numeric(n_cols); ir_vec <- numeric(n_cols)

for(i in 1:n_cols){
  col_data <- ts_ret[, i]
  b <- CAPM.beta(Ra = col_data, Rb = market_ret, Rf = ts_rf)
  t <- TreynorRatio(Ra = col_data, Rb = market_ret, Rf = ts_rf, scale = 12)
  ir <- InformationRatio(Ra = col_data, Rb = market_ret, scale = 12)
  beta_vec[i]    <- if(is.finite(b)) b else NA
  treynor_vec[i] <- if(is.finite(t)) t else NA
  ir_vec[i]      <- if(is.finite(ir)) ir else NA
}

perf_matrix <- rbind(tab_ann, 
                     "Max Drawdown" = max_dd, 
                     "Sortino Ratio" = sortino, 
                     "Calmar Ratio" = calmar, 
                     "Beta (vs DJITR)" = matrix(beta_vec, nrow=1, dimnames=list(NULL, ts_names)), 
                     "Treynor Ratio" = matrix(treynor_vec, nrow=1, dimnames=list(NULL, ts_names)), 
                     "Information Ratio" = matrix(ir_vec, nrow=1, dimnames=list(NULL, ts_names)))

df_perf <- as.data.frame(perf_matrix)
df_perf <- df_perf %>% mutate(across(everything(), ~ ifelse(is.na(.), "", as.character(round(as.numeric(.), 4)))))
df_perf <- cbind(Metric = rownames(perf_matrix), df_perf)

# 繪圖 (起始點 0 校正)
ts_plot_core <- xts(coredata(ts_ret), order.by = portfolio_ts$Expiry_Date_Ref)
ts_start <- xts(matrix(0, 1, 4), order.by = min(portfolio_ts$Date_For_Port))
colnames(ts_start) <- colnames(ts_plot_core)
ts_ret_plot <- rbind(ts_start, ts_plot_core)

temp_plot <- paste0(path_input_folder, "MBXM_Portfolio_Chart.png")
png(temp_plot, width = 1400, height = 800, res = 120)
my_colors <- c("darkgreen", "#778899", "gray", "black") # MBXM 使用深綠色
charts.PerformanceSummary(ts_ret_plot, wealth.index = TRUE, main = "MBXM Performance (Benchmark: DJITR)", 
                          legend.loc = "topleft", colorset = my_colors, lwd = 2)
dev.off()

# 輸出 Excel
wb <- createWorkbook()
addWorksheet(wb, "Portfolio_Log"); writeData(wb, "Portfolio_Log", portfolio_ts)
addWorksheet(wb, "Performance");   writeData(wb, "Performance", df_perf)
insertImage(wb, "Performance", temp_plot, width = 10, height = 6, startRow = 20, startCol = 2)
saveWorkbook(wb, file.path(path_input_folder, output_filename), overwrite = TRUE)

if(file.exists(temp_plot)) file.remove(temp_plot)
message(paste("🎉 MBXM 價格權重投組報表已完成，路徑:", file.path(path_input_folder, output_filename)))