# ==========================================
# CBXM Strategy Signal: Aligned with 3rd Fridays
# ==========================================

library(tidyverse)
library(zoo)
library(lubridate)
library(readxl)

# 1. Load Data
file_path <- "C:/Users/User/Desktop/Thesis_project/data_raw/VIX.xls"
vix_daily <- read_excel(file_path) %>%
  rename(RawDate = 1, VIX = 2) %>%
  mutate(Date = if(is.numeric(RawDate)) as.Date(RawDate, origin = "1899-12-30") else as.Date(RawDate)) %>%
  select(Date, VIX) %>% filter(!is.na(VIX))

# 2. Generate Sequence of 3rd Fridays (Trading Nodes)
all_dates <- seq(as.Date("2003-01-01"), as.Date("2024-12-31"), by="day")
third_fridays <- all_dates[wday(all_dates) == 6] %>% 
  as.data.frame() %>% rename(Date = 1) %>%
  mutate(year = year(Date), month = month(Date)) %>%
  group_by(year, month) %>% slice(3) %>% ungroup() %>% pull(Date)

# 3. Establish Cycle Statistics
vix_cycles <- data.frame(
  Start = lag(third_fridays),
  End   = third_fridays - days(1),
  Execution_Date = third_fridays
) %>% na.omit() %>%
  rowwise() %>%
  mutate(
    Cycle_Median = {
      val <- vix_daily$VIX[vix_daily$Date >= Start & vix_daily$Date <= End]
      if(length(val) > 0) median(val, na.rm=TRUE) else NA
    }
  ) %>% ungroup()

# 4. Generate the 3-Tier Timeline Table
vix_signal_final <- vix_cycles %>%
  mutate(
    # Benchmark: Rolling median of the last 12 cycles (including current)
    Rolling_Median = rollapplyr(Cycle_Median, width = 12, FUN = median, fill = NA),
    
    # The benchmark value visible on Execution_Date (the previous cycle's rolling result)
    Benchmark_Value = lag(Rolling_Median, 1),
    
    # Format Time Windows
    Current_Window = paste0(Start, " ~ ", End),
    Benchmark_Window = paste0(lag(Start, 12), " ~ ", lag(End, 1))
  ) %>%
  # Determine Signal (Look-ahead Bias Free)
  mutate(Is_High_Vol = if_else(Cycle_Median > Benchmark_Value, 1, 0)) %>%
  # Filter out the initial year where data is insufficient for a 12-month benchmark
  filter(!is.na(Benchmark_Value) & !is.na(lag(Start, 12))) %>%
  select(
    Execution_Date,
    Current_Window,
    Current_Median = Cycle_Median,
    Benchmark_Window,
    Benchmark_Median = Benchmark_Value,
    Is_High_Vol
  )

# 5. Export
print(head(vix_signal_final))
write_csv(vix_signal_final, "C:/Users/User/Desktop/Thesis_project/data_raw/VIX_CBXM.csv")