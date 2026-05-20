# ==============================================================================
# 最終強健版：DBXM 投資組合整合 (價格權重聚合)
# 解決 rbind 報錯與日期解析警告，完全對齊 SBXM 規格
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
path_db             <- "C:/Users/User/Desktop/Thesis_project/Data/SQLDB1.sqlite" 
path_input_folder   <- "C:/Users/User/Desktop/Thesis_project/Output_DBXM/"
path_dia_file       <- "C:/Users/User/Desktop/Thesis_project/data_raw/DIA.xls"
path_djitr_file     <- "C:/Users/User/Desktop/Thesis_project/data_raw/DJI_還原.xls"
output_filename     <- "DBXM_Portfolio.xlsx"

Sys.setlocale("LC_TIME", "C") 

clean_excel_date <- function(x) {
  if (is.null(x)) return(as.Date(NA))
  if (is.numeric(x)) as.Date(x, origin = "1899-12-30") else as.Date(x)
}

get_price_fn <- function(query_date, price_df) {
  if (is.null(price_df) || nrow(price_df) == 0) return(NA)
  subset <- price_df %>% filter(Date <= query_date)
  if (nrow(subset) == 0) return(NA)
  return(tail(subset$Close, 1))
}

# ==============================================================================
# 3. 讀取 SQL 與大盤資料
# ==============================================================================
message("🔍 連線 SQL 資料庫...")
con <- dbConnect(RSQLite::SQLite(), path_db)

djitr_clean <- tryCatch({
  dbReadTable(con, "dji_index") %>% rename(Date = 1, Close = 2) %>%
    mutate(Date = as.Date(Date), Close = as.numeric(Close)) %>% arrange(Date)
}, error = function(e) { message("⚠️ SQL 大盤資料讀取失敗"); data.frame() })

dia_clean <- if(file.exists(path_dia_file)) {
  read_excel(path_dia_file) %>% select(1, 2) %>% setNames(c("Date", "Close")) %>%
    mutate(Date = clean_excel_date(Date), Close = as.numeric(Close)) %>% arrange(Date)
} else { data.frame() }

# ==============================================================================
# 4. Step 1: 讀取個股回測結果 (改用 bind_rows 解決行數無效錯誤)
# ==============================================================================
message("🔍 Step 1: 讀取個股回測檔...")
file_list <- list.files(path_input_folder, pattern = "(DBXM|MBXM)_.*\\.xlsx", full.names = TRUE)
file_list <- file_list[!grepl("Portfolio", basename(file_list))] 

stock_list <- list() # 改用 list 收集再合併

for (f in file_list) {
  if (grepl("^~\\$", basename(f))) next 
  
  ticker <- str_match(basename(f), "(?:DBXM|MBXM)_(.*)\\.xlsx")[2]
  
  df_tmp <- tryCatch({
    tmp <- readxl::read_excel(f, sheet = "Trade_Log")
    names(tmp) <- tolower(names(tmp))
    
    # 策略報酬欄位兼容處理 (對應 dbxm_return 或 mbxm_return)
    if ("dbxm_return" %in% names(tmp)) {
      tmp$strat_ret <- tmp$dbxm_return
    } else if ("mbxm_return" %in% names(tmp)) {
      tmp$strat_ret <- tmp$mbxm_return
    } else {
      stop("找不到報酬率欄位")
    }
    
    tmp %>% mutate(
      Date_For_Port = clean_excel_date(trading_date),
      Date_Expiry   = clean_excel_date(expiry_date),
      Ticker = ticker,
      # 價值結算邏輯
      Term_Value_DBXM = close_t1 + payoff + dividend,
      Implied_Cost_DBXM = Term_Value_DBXM / (1 + strat_ret),
      Term_Value_BnH = close_t1 + dividend,
      Implied_Cost_BnH = Term_Value_BnH / (1 + bnh_return)
    ) %>% filter(!is.na(strat_ret))
  }, error = function(e) { message(paste("⚠️ 讀取失敗:", ticker, e$message)); NULL })
  
  if (!is.null(df_tmp)) stock_list[[ticker]] <- df_tmp
}

# 關鍵修正：使用 bind_rows 進行強健合併
stock_data <- bind_rows(stock_list)

if (nrow(stock_data) == 0) stop("❌ 錯誤：找不到任何有效的個股資料。")

# ==============================================================================
# 5. Step 2: 成分股匹配 (解決解析失敗問題)
# ==============================================================================
message("🔍 Step 2: 處理 SQL 成分股動態匹配...")
components_raw <- dbReadTable(con, "dji_constituents_raw")
dbDisconnect(con)

names(components_raw)[tolower(names(components_raw)) == "ticker"] <- "Ticker"
date_cols <- grep("Equity", names(components_raw), value = TRUE)
date_cols <- date_cols[date_cols != "Latest % Of Equity"]

components_db <- components_raw %>%
  select(Ticker, all_of(date_cols)) %>%
  pivot_longer(cols = -Ticker, names_to = "Date_String", values_to = "Weight") %>%
  filter(!is.na(Weight) & Weight > 0) %>%
  mutate(
    Clean_Date = str_replace_all(Date_String, "[^a-zA-Z0-9]", " "),
    Clean_Date = str_trim(str_remove_all(Clean_Date, "(?i)of equity")),
    # 強化日期解析
    Comp_Date_Obj = as.Date(parse_date_time(Clean_Date, orders = c("mdy", "bdy", "dmy", "ymd"), quiet = TRUE))
  ) %>%
  filter(!is.na(Comp_Date_Obj))

unique_dates <- sort(unique(stock_data$Date_For_Port))
comp_dates   <- sort(unique(components_db$Comp_Date_Obj))

match_table <- data.frame(Trade_Date = unique_dates)
match_table$Target_Comp_Date <- as.Date(sapply(unique_dates, function(d) {
  past_dates <- comp_dates[comp_dates <= d]
  if(length(past_dates) > 0) max(past_dates) else min(comp_dates)
}), origin = "1970-01-01")

portfolio_data <- stock_data %>%
  left_join(match_table, by = c("Date_For_Port" = "Trade_Date")) %>%
  inner_join(components_db, by = c("Ticker" = "Ticker", "Target_Comp_Date" = "Comp_Date_Obj"))

# ==============================================================================
# 6. Step 3: 計算投組績效
# ==============================================================================
message("🔄 Step 3: 計算價格權重投組聚合績效...")

portfolio_ts <- portfolio_data %>%
  group_by(Date_For_Port) %>%
  summarise(
    Component_Count = n(),
    Expiry_Ref = first(Date_Expiry),
    Cost_DBXM = sum(Implied_Cost_DBXM, na.rm=T), Value_DBXM = sum(Term_Value_DBXM, na.rm=T),
    Cost_BnH = sum(Implied_Cost_BnH, na.rm=T),  Value_BnH = sum(Term_Value_BnH, na.rm=T),
    Avg_Rf = mean(riskfreerate_30d, na.rm=T)
  ) %>%
  rowwise() %>%
  mutate(
    DBXM_Return = (Value_DBXM / Cost_DBXM) - 1,
    BnH_Return  = (Value_BnH / Cost_BnH) - 1,
    DIA_Return  = (get_price_fn(Expiry_Ref, dia_clean) / get_price_fn(Date_For_Port, dia_clean)) - 1,
    DJITR_Return = (get_price_fn(Expiry_Ref, djitr_clean) / get_price_fn(Date_For_Port, djitr_clean)) - 1
  ) %>%
  ungroup() %>% filter(!is.na(DBXM_Return))

# ==============================================================================
# 7. Step 6: 輸出與繪圖 (完全對齊標準規格)
# ==============================================================================
message("📊 Step 6: 產出報表與績效矩陣...")

ts_cols  <- c("DBXM_Return", "BnH_Return", "DIA_Return", "DJITR_Return")
ts_names <- c("DBXM(M=1)", "BnH", "DIA(ETF)", "DJITR")
ts_ret   <- xts(portfolio_ts[, ts_cols], order.by = portfolio_ts$Date_For_Port)
colnames(ts_ret) <- ts_names
ts_rf <- xts(portfolio_ts$Avg_Rf / 1200, order.by = portfolio_ts$Date_For_Port)

# 指標計算
tab_ann <- table.AnnualizedReturns(ts_ret, Rf = ts_rf, scale = 12)
mdd <- maxDrawdown(ts_ret); sr <- SortinoRatio(ts_ret, MAR=0); cr <- CalmarRatio(ts_ret, scale=12)

# Beta, Treynor, IR
market_ret <- ts_ret[, "DJITR"]
beta_v <- sapply(1:4, function(i) CAPM.beta(ts_ret[,i], market_ret, ts_rf))
trey_v <- sapply(1:4, function(i) TreynorRatio(ts_ret[,i], market_ret, ts_rf, scale=12))
ir_v   <- sapply(1:4, function(i) InformationRatio(ts_ret[,i], market_ret, scale=12))

perf_matrix <- rbind(tab_ann, "Max Drawdown"=mdd, "Sortino Ratio"=sr, "Calmar Ratio"=cr, 
                     "Beta (vs DJITR)"=matrix(beta_v, 1, dimnames=list(NULL, ts_names)),
                     "Treynor Ratio"=matrix(trey_v, 1, dimnames=list(NULL, ts_names)),
                     "Information Ratio"=matrix(ir_v, 1, dimnames=list(NULL, ts_names)))

df_perf <- as.data.frame(perf_matrix) %>% 
  mutate(across(everything(), ~ ifelse(is.na(.) | !is.finite(as.numeric(.)), "", as.character(round(as.numeric(.), 4)))))
df_perf <- cbind(Metric = rownames(perf_matrix), df_perf)

# 繪圖
ts_ret_plot <- rbind(xts(matrix(0, 1, 4), order.by=min(portfolio_ts$Date_For_Port)), 
                     xts(coredata(ts_ret), order.by=portfolio_ts$Expiry_Ref))
colnames(ts_ret_plot) <- ts_names

temp_plot <- file.path(path_input_folder, "DBXM_Portfolio_Chart.png")
png(temp_plot, width = 1400, height = 800, res = 120)
charts.PerformanceSummary(ts_ret_plot, wealth.index=TRUE, main="DBXM Performance (Benchmark: DJITR)", 
                          legend.loc="topleft", colorset=c("red", "#778899", "grey", "black"), lwd=2)
dev.off()

# Excel 輸出
wb <- createWorkbook()
addWorksheet(wb, "Portfolio_Log"); writeData(wb, "Portfolio_Log", portfolio_ts)
addWorksheet(wb, "Performance");   writeData(wb, "Performance", df_perf)
insertImage(wb, "Performance", temp_plot, width=10, height=6, startRow=22, startCol=2)
saveWorkbook(wb, file.path(path_input_folder, output_filename), overwrite = TRUE)

if(file.exists(temp_plot)) file.remove(temp_plot)
message(paste("🎉 DBXM 價格權重報表已完美輸出:", output_filename))