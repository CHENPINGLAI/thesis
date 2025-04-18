### 載入所需套件
install.packages("devtools")
library(devtools)
install.packages("optimx", dependencies = TRUE)
install_github("mpiktas/midasr", force = TRUE) # 選1
install.packages(c("sandwich", "optimx", "quantreg", "SparseM"))
install.packages("quantmod")
library(optimx)
library(zoo)
library(quantmod)
library(midasr)
library(quantmod)
library(lubridate)
library(forecast)
library(dplyr)
library(readr)
library(ggplot2)


### 匯入所需資料
# 匯入台灣gdp_data
file_path_gdp <- "台灣gdp_data.csv"
original_gdp_data <- read.csv(file_path_gdp, fileEncoding = "big5", encoding = "UTF-8")

# gdp的資料有整年度全部資料及各季資料，因此利用grep()先將每季的資料篩選出來
total_gdp_data <- original_gdp_data[grep("第", original_gdp_data$統計期), ]

### 選取台灣gdp資料時間為1999Q1-2019Q4（總共84季）
total_gdp_data <- total_gdp_data %>%
  mutate(
    # 將民國年份轉為西元年份
    年份 = ifelse(
      # 判斷年份是否為三位數（例如 100 年）
      # ^：字串的開頭
      # \\d{3}：匹配恰好三個數字
      grepl("^\\d{3}", 統計期),
      as.numeric(substr(統計期, 1, 3)) + 1911, # 三位數民國年份轉換
      as.numeric(substr(統計期, 1, 2)) + 1911  # 兩位數民國年份轉換
    ),
    # 取得季度資訊
    季度 = substr(統計期, nchar(統計期) - 2, nchar(統計期))
  ) %>%
  # 選取年份從 1999 年~ 2019 年
  filter(年份 >= 1999 & 年份 <= 2019)

total_gdp_data <- total_gdp_data[, c(1,2)]

# 台灣gdp_data的敘述性統計
dim(total_gdp_data) # 84  2
summary(total_gdp_data$平均每人GDP.名目值.元.)
sd(total_gdp_data$平均每人GDP.名目值.元.) # 28606.33

# 匯入台灣cpi_data
file_path_cpi <- "台灣cpi_data.csv"
original_cpi_data <- read.csv(file_path_cpi, fileEncoding = "big5", encoding = "UTF-8")

# cpi的資料有整年度全部資料及各季資料，因此利用grep()先將每季的資料篩選出來
# 篩選出每個月的cpi資料
total_cpi_data <- original_cpi_data[grep("月", original_cpi_data$統計期), c(1, 2)]

### 選取台灣cpi資料時間為1999M1-2019M12（總共84季，252個月）
total_cpi_data <- total_cpi_data %>%
  mutate(
    # 將民國年份轉為西元年份
    年份 = ifelse(
      grepl("^\\d{3}", 統計期), # 判斷年份是否為三位數（例如 100 年）
      as.numeric(substr(統計期, 1, 3)) + 1911, # 三位數民國年份轉換
      as.numeric(substr(統計期, 1, 2)) + 1911  # 兩位數民國年份轉換
    ),
    # 取得月份資訊，i.e.將 "1月", "12月" 等字串轉為數字 1, 2, ..., 12
    # gsub(pattern, "", x, ...)：將字串移除
    # [^0-9]：將字串中除了數字的其他符號移除
    月份 = as.numeric(gsub("[^0-9]", "", substr(統計期, nchar(統計期) - 2, nchar(統計期))))
  ) %>%
  # 選取年份從 1999 年~ 2019 年
  filter(年份 >= 1999 & 年份 <= 2019)

total_cpi_data <- total_cpi_data[, c(1, 2)]

# 台灣cpi_data的敘述性統計
dim(total_cpi_data) # 252   2
summary(total_cpi_data$總指數)
sd(total_cpi_data$總指數) # 5.938007



# 匯入台灣10年國債利率DGS10_data
original_DGS10_data <- read_csv("台灣DGS10_data.csv")
original_DGS10_data <- original_DGS10_data[, c(1,2)]

original_DGS10_data$日期 <- as.Date(original_DGS10_data$日期)
original_DGS10_data$YearQuarter <- paste(format(original_DGS10_data$日期, "%Y"), quarters(original_DGS10_data$日期))
original_DGS10_data <- original_DGS10_data %>%
                            arrange(日期) 

original_DGS10_data <- original_DGS10_data %>%
  filter(日期 >= as.Date("1999-01-01") & 日期 <= as.Date("2019-12-31"))

tail(original_DGS10_data)
dim(original_DGS10_data) # 5355    3
summary(original_DGS10_data$收市)
sd(original_DGS10_data$收市)


### 隨機補值（補成66個值/1季）
### 從1999/01/13~2019/12/31（總共84季，應該為5544天），僅5349個值，
### 因此需要補成5544個值，每季個數應為66個值

# 找到所有唯一的季度
unique_quarters <- unique(original_DGS10_data$YearQuarter)

# 初始化一個空的資料框
total_DGS10_data <- data.frame()

# 遍歷每個季度
for (quarter in unique_quarters) {
  # 找出當季資料
  current_data <- original_DGS10_data[original_DGS10_data$YearQuarter == quarter, ]
  current_data$日期 <- as.Date(current_data$日期)
  
  # 計算該季的資料數量
  current_count <- nrow(current_data)
  
  if (current_count < 66) {
    # 若資料不足 66 個，進行隨機補值
    year <- substr(quarter, 1, 4)
    quarter_start <- as.Date(paste0(year, "-", switch(substr(quarter, 6, 7),
                                                      "Q1" = "01-01",
                                                      "Q2" = "04-01",
                                                      "Q3" = "07-01",
                                                      "Q4" = "10-01")))
    quarter_end <- quarter_start + 90 - 1
    earliest_date <- min(current_data$日期)
    all_possible_dates <- seq(quarter_start, quarter_end, by = "day")
    valid_dates <- all_possible_dates[all_possible_dates >= earliest_date]
    missing_dates <- setdiff(valid_dates, current_data$日期)
    missing_count <- 66 - current_count
    random_dates <- sample(missing_dates, size = missing_count, replace = FALSE)
    
    # 填補隨機日期及 NA 值
    new_rows <- data.frame(
      日期 = random_dates,
      收市 = NA,  
      YearQuarter = quarter
    )
    
    # 合併並排序
    current_data <- rbind(current_data, new_rows)
    current_data <- current_data[order(current_data$日期), ]
    
    # 使用前一天的 DGS10 值補充 NA
    library(zoo)
    current_data$收市 <- na.locf(current_data$收市)
    
  } else if (current_count > 66) {
    # 若資料超過 66 個，隨機移除多餘的行
    excess_count <- current_count - 66
    remove_indices <- sample(seq_len(current_count), size = excess_count, replace = FALSE)
    current_data <- current_data[-remove_indices, ]
  }
  
  # 將處理後的季度數據合併到最終結果
  total_DGS10_data <- rbind(total_DGS10_data, current_data)
}

total_DGS10_data <- total_DGS10_data[total_DGS10_data$日期 >= as.Date("1999-01-01") &
                                    total_DGS10_data$日期 <= as.Date("2019-12-31"), ]

# 查看結果
head(total_DGS10_data)

# 台灣十年國債資料的敘述性統計
dim(total_DGS10_data) # 5544    3
summary(total_DGS10_data$收市)
sd(total_DGS10_data$收市)


# 確認每一季的資料總共為66筆
check_DGS10_quarter <- total_DGS10_data %>%  
  group_by(YearQuarter) %>%
  summarise(Count = n())

which(check_DGS10_quarter$Count != 66) # 確認每一季資料都有66個值（補值成功）



### 將宏觀經濟指標進行資料前處理
# GDP取一階diff：
total_gdp_data$gdp_log_diff <- c(NA, diff(log(total_gdp_data$平均每人GDP.名目值.元.)))
# CPI取一階diff：
total_cpi_data$cpi_log_diff <- c(NA, diff(log(total_cpi_data$總指數)))



### 將這三個資料，轉換成time series模式，起始為1999年Q1
# gdp_ts：1999Q1~2019Q4
gdp_ts <- ts(c(as.numeric(total_gdp_data$gdp_log_diff)), start=c(1999,1), frequency = 4) 
# cpi_ts：1999M1~2019M12
cpi_ts <- ts(c(as.numeric(total_cpi_data$cpi_log_diff)), start=c(1999,1), frequency = 12)
# interest_rate_ts：1999/01/01~2019/12/31
interest_rate_ts <- ts(c(as.numeric(total_DGS10_data$收市)), start=c(1999,1), frequency = 22*12)



### rolling forcast window
### 創造gdp的 rolling forcast window
gdp_start_time <- c(1999, 1) # 1999Q1
gdp_end_time <- c(2019, 4)   # 2019Q4

# 生成每一季的時間點
gdp_time_points <- seq(from = gdp_start_time[1] + (gdp_start_time[2]-1) / 4,
                   to = gdp_end_time[1] + (gdp_end_time[2]-1) / 4,
                   by = 0.25)

# 切分每一個in_sample及out_of_sample的rolling forecast window
gdp_in_sample <- list()

for (i in seq_len(34)) {
  gdp_in_sample[[i]] <- window(gdp_ts, start = gdp_time_points[i], end = gdp_time_points[i + 49])
}

gdp_out_of_sample <- list()

for (i in seq_len(34)) {
  gdp_out_of_sample[[i]] <- window(gdp_ts, start = gdp_time_points[i], end = gdp_time_points[i + 50])
}

gdp_in_sample
gdp_out_of_sample



### 創造cpi的 rolling forcast window
cpi_start_time <- c(1999, 1) # 1999年1月
cpi_end_time <- c(2019, 12)  # 2019年12月

# 生成每個月的時間點
cpi_time_points <- seq(from = cpi_start_time[1] + (cpi_start_time[2] - 1) / 12,
                   to = cpi_end_time[1] + (cpi_end_time[2] - 1) / 12,
                   by = 1 / 12) 


cpi_in_sample <- list()

for (i in seq_len(34)) {
  cpi_in_sample[[i]] <- window(cpi_ts, start = cpi_time_points[1+3*(i-1)], end = cpi_time_points[1+3*(i-1)+149])
}

cpi_out_of_sample <- list()

for (i in seq_len(34)) {
  cpi_out_of_sample[[i]] <- window(cpi_ts, start = cpi_time_points[1+3*(i-1)], end = cpi_time_points[1+3*(i-1)+152])
}

cpi_in_sample
cpi_out_of_sample


### 創造interest_rate的 rolling forcast window
interest_rate_start_time <- c(1999, 1)   # 1999年1月1日
interest_rate_end_time <- c(2019, 22*12) # 2019年12月31日

# 生成每個月的時間點
interest_rate_time_points <- seq(from = interest_rate_start_time[1] + (interest_rate_start_time[2] - 1) / (66*4),
                   to = interest_rate_end_time[1] + (interest_rate_end_time[2] - 1) / (66*4),
                   by = 1 / (66*4)) 


interest_rate_in_sample <- list()

for (i in seq_len(34)) {
  interest_rate_in_sample[[i]] <- window(interest_rate_ts, start = interest_rate_time_points[1+66*(i-1)], end = interest_rate_time_points[1+66*(i-1)+(66*50-1)])
}

interest_rate_out_of_sample <- list()

for (i in seq_len(34)) {
  interest_rate_out_of_sample[[i]] <- window(interest_rate_ts, start = interest_rate_time_points[1+66*(i-1)], end = interest_rate_time_points[1+66*(i-1)+(66*51-1)])
}

interest_rate_in_sample
interest_rate_out_of_sample


### 跑RR-MIDAS、RU-MIDAS、僅interest rate only預測模型
m_d <- 66
m_m <- 3

### 定義weighted function：custom_nealmon（仿造midasr套件中的nealmon函式所寫）
custom_nealmon <- function(p, d, m) {
  i <- 1:d
  pl <- (poly(i, degree = length(p) - 1, raw = TRUE)**2) %*% p[-1]
  
  # 希望數值穩定，所以將此weighted function分子、分母同除exp(最大值)
  max_pl <- max(pl)  
  pl_shifted <- pl - max_pl  # 將pl平移(減去最大值)
  exp_pl <- exp(pl_shifted)
  as.vector(p[1] * exp_pl / sum(exp_pl))
}


### 因為三筆資料更新時間不同，所以將資料分成
### h= 1-5、h = 6-27、h = 28-44、h = 45-49、h = 50-66做訓練及預測結果
# 此為h = 1-5
rr_pred_1_to_5 <- c()
ru_pred_1_to_5 <- c()
interest_rate_only_pred_1_to_5 <- c()
real_value_1_to_5 <- c()
converge_1_to_5 <- c()
for (i in 1:34){
  for (h in 1:5){
    # 真實資料
    real <- mls(interest_rate_out_of_sample[[i]], (m_d - h), m_d)[51]
    real_value_1_to_5 <- c(real_value_1_to_5, real)
    
    # RU-MIDAS
    ru <- midas_r(mls(interest_rate_ts_window_1, (m_d - h), m_d) ~ 
                    mls(interest_rate_ts_window_1, (m_d - h + 1) + 0:21, m_d) + 
                    mls(cpi_ts_window_1,  4:15, m_m) + 
                    mls(gdp_ts_window_1, 2:5, 1) , 
                  data = list(interest_rate_ts_window_1 = interest_rate_in_sample[[i]],
                              cpi_ts_window_1 = cpi_in_sample[[i]], 
                              gdp_ts_window_1 = gdp_in_sample[[i]]), start = NULL)
    pred_ru <- predict(ru, newdata = list(interest_rate_ts_window_1 = interest_rate_out_of_sample[[i]],
                                          cpi_ts_window_1 = cpi_out_of_sample[[i]], 
                                          gdp_ts_window_1 = gdp_out_of_sample[[i]]))
    ru_pred_1_to_5 <- c(ru_pred_1_to_5, pred_ru[length(pred_ru)])
    
    # RR-MIDAS
    rr <- midas_r(mls(interest_rate_ts_window_1, (m_d - h), m_d) ~ 
                    mls(interest_rate_ts_window_1, (m_d - h + 1) + 0:21, m_d, custom_nealmon) + 
                    mls(cpi_ts_window_1, 4:15, m_m, custom_nealmon) + 
                    mls(gdp_ts_window_1, 2:5, 1, custom_nealmon) , 
                  data = list(interest_rate_ts_window_1 = interest_rate_in_sample[[i]],
                              cpi_ts_window_1 = cpi_in_sample[[i]], 
                              gdp_ts_window_1 = gdp_in_sample[[i]]), 
                  start = list(interest_rate_ts_window_1 = c(1, -0.05),
                               cpi_ts_window_1 = c(1, -0.05),
                               gdp_ts_window_1 = c(1, -0.05)), method = 'BFGS',
                  control = list(maxit = 10000))
    
    pred_rr <- predict(rr, newdata = list(interest_rate_ts_window_1 = interest_rate_out_of_sample[[i]],
                                          cpi_ts_window_1 = cpi_out_of_sample[[i]], 
                                          gdp_ts_window_1 = gdp_out_of_sample[[i]]))
    rr_pred_1_to_5 <- c(rr_pred_1_to_5, pred_rr[length(pred_rr)])
    converge_1_to_5 <- c(converge_1_to_5, rr$convergence)
    
    # 僅使用interest rate所做的interest_rate_only model
    interest_rate_only <- lm(mls(interest_rate_ts_window_1, (m_d - h), m_d) ~ 
                                    mls(interest_rate_ts_window_1, (m_d - h + 1) + 0:21, m_d), 
                                  data = list(interest_rate_ts_window_1 = interest_rate_in_sample[[i]]))
    pred_interest_rate_only <- predict(interest_rate_only, newdata = list(interest_rate_ts_window_1 = interest_rate_out_of_sample[[i]]))
    interest_rate_only_pred_1_to_5 <- c(interest_rate_only_pred_1_to_5, pred_interest_rate_only[length(pred_interest_rate_only)])
  }
}


# 此為h = 6-27
rr_pred_6_to_27 <- c()
ru_pred_6_to_27 <- c()
interest_rate_only_pred_6_to_27 <- c()
real_value_6_to_27 <- c()
converge_6_to_27 <- c()
for (i in 1:34){
  for (h in 6:27){
    # 真實資料
    real <- mls(interest_rate_out_of_sample[[i]], (m_d - h), m_d)[51]
    real_value_6_to_27 <- c(real_value_6_to_27, real)
    
    # RU-MIDAS
    ru <- midas_r(mls(interest_rate_ts_window_1, (m_d - h), m_d) ~ 
                    mls(interest_rate_ts_window_1, (m_d - h + 1) + 0:21, m_d) + 
                    mls(cpi_ts_window_1,  3:14, m_m) + 
                    mls(gdp_ts_window_1, 2:5, 1) , 
                  data = list(interest_rate_ts_window_1 = interest_rate_in_sample[[i]],
                              cpi_ts_window_1 = cpi_in_sample[[i]], 
                              gdp_ts_window_1 = gdp_in_sample[[i]]), start = NULL)
    pred_ru <- predict(ru, newdata = list(interest_rate_ts_window_1 = interest_rate_out_of_sample[[i]],
                                          cpi_ts_window_1 = cpi_out_of_sample[[i]], 
                                          gdp_ts_window_1 = gdp_out_of_sample[[i]]))
    ru_pred_6_to_27 <- c(ru_pred_6_to_27, pred_ru[length(pred_ru)])
    
    # RR-MIDAS
    rr <- midas_r(mls(interest_rate_ts_window_1, (m_d - h), m_d) ~ 
                    mls(interest_rate_ts_window_1, (m_d - h + 1) + 0:21, m_d, custom_nealmon) + 
                    mls(cpi_ts_window_1, 3:14, m_m, custom_nealmon) + 
                    mls(gdp_ts_window_1, 2:5, 1, custom_nealmon) , 
                  data = list(interest_rate_ts_window_1 = interest_rate_in_sample[[i]],
                              cpi_ts_window_1 = cpi_in_sample[[i]], 
                              gdp_ts_window_1 = gdp_in_sample[[i]]), 
                  start = list(interest_rate_ts_window_1 = c(1, -0.05),
                               cpi_ts_window_1 = c(1, -0.05),
                               gdp_ts_window_1 = c(1, -0.05)), method = 'BFGS',
                  control = list(maxit = 10000))
    
    pred_rr <- predict(rr, newdata = list(interest_rate_ts_window_1 = interest_rate_out_of_sample[[i]],
                                          cpi_ts_window_1 = cpi_out_of_sample[[i]], 
                                          gdp_ts_window_1 = gdp_out_of_sample[[i]]))
    rr_pred_6_to_27 <- c(rr_pred_6_to_27, pred_rr[length(pred_rr)])
    converge_6_to_27 <- c(converge_6_to_27, rr$convergence)
    
    # 僅使用interest rate所做的interest_rate_only model
    interest_rate_only <- lm(mls(interest_rate_ts_window_1, (m_d - h), m_d) ~ 
                               mls(interest_rate_ts_window_1, (m_d - h + 1) + 0:21, m_d), 
                             data = list(interest_rate_ts_window_1 = interest_rate_in_sample[[i]]))
    pred_interest_rate_only <- predict(interest_rate_only, newdata = list(interest_rate_ts_window_1 = interest_rate_out_of_sample[[i]]))
    interest_rate_only_pred_6_to_27 <- c(interest_rate_only_pred_6_to_27, pred_interest_rate_only[length(pred_interest_rate_only)])
  }
}


# 此為h = 28-44
rr_pred_28_to_44 <- c()
ru_pred_28_to_44 <- c()
interest_rate_only_pred_28_to_44 <- c()
real_value_28_to_44 <- c()
converge_28_to_44 <- c()
for (i in 1:34){
  for (h in 28:44){
    # 真實資料
    real <- mls(interest_rate_out_of_sample[[i]], (m_d - h), m_d)[51]
    real_value_28_to_44 <- c(real_value_28_to_44, real)
    
    # RU-MIDAS
    ru <- midas_r(mls(interest_rate_ts_window_1, (m_d - h), m_d) ~ 
                    mls(interest_rate_ts_window_1, (m_d - h + 1) + 0:21, m_d) + 
                    mls(cpi_ts_window_1,  2:13, m_m) + 
                    mls(gdp_ts_window_1, 2:5, 1) , 
                  data = list(interest_rate_ts_window_1 = interest_rate_in_sample[[i]],
                              cpi_ts_window_1 = cpi_in_sample[[i]], 
                              gdp_ts_window_1 = gdp_in_sample[[i]]), start = NULL)
    pred_ru <- predict(ru, newdata = list(interest_rate_ts_window_1 = interest_rate_out_of_sample[[i]],
                                          cpi_ts_window_1 = cpi_out_of_sample[[i]], 
                                          gdp_ts_window_1 = gdp_out_of_sample[[i]]))
    ru_pred_28_to_44 <- c(ru_pred_28_to_44, pred_ru[length(pred_ru)])
    
    # RR-MIDAS
    rr <- midas_r(mls(interest_rate_ts_window_1, (m_d - h), m_d) ~ 
                    mls(interest_rate_ts_window_1, (m_d - h + 1) + 0:21, m_d, custom_nealmon) + 
                    mls(cpi_ts_window_1, 2:13, m_m, custom_nealmon) + 
                    mls(gdp_ts_window_1, 2:5, 1, custom_nealmon) , 
                  data = list(interest_rate_ts_window_1 = interest_rate_in_sample[[i]],
                              cpi_ts_window_1 = cpi_in_sample[[i]], 
                              gdp_ts_window_1 = gdp_in_sample[[i]]), 
                  start = list(interest_rate_ts_window_1 = c(1, -0.05),
                               cpi_ts_window_1 = c(1, -0.05),
                               gdp_ts_window_1 = c(1, -0.05)), method = 'BFGS',
                  control = list(maxit = 10000))
    
    pred_rr <- predict(rr, newdata = list(interest_rate_ts_window_1 = interest_rate_out_of_sample[[i]],
                                          cpi_ts_window_1 = cpi_out_of_sample[[i]], 
                                          gdp_ts_window_1 = gdp_out_of_sample[[i]]))
    rr_pred_28_to_44 <- c(rr_pred_28_to_44, pred_rr[length(pred_rr)])
    converge_28_to_44 <- c(converge_28_to_44, rr$convergence)
    
    # 僅使用interest rate所做的interest_rate_only model
    interest_rate_only <- lm(mls(interest_rate_ts_window_1, (m_d - h), m_d) ~ 
                               mls(interest_rate_ts_window_1, (m_d - h + 1) + 0:21, m_d), 
                             data = list(interest_rate_ts_window_1 = interest_rate_in_sample[[i]]))
    pred_interest_rate_only <- predict(interest_rate_only, newdata = list(interest_rate_ts_window_1 = interest_rate_out_of_sample[[i]]))
    interest_rate_only_pred_28_to_44 <- c(interest_rate_only_pred_28_to_44, pred_interest_rate_only[length(pred_interest_rate_only)])
  }
}


# 此為h = 45-49
rr_pred_45_to_49 <- c()
ru_pred_45_to_49 <- c()
interest_rate_only_pred_45_to_49 <- c()
real_value_45_to_49 <- c()
converge_45_to_49 <- c()
for (i in 1:34){
  for (h in 45:49){
    # 真實資料
    real <- mls(interest_rate_out_of_sample[[i]], (m_d - h), m_d)[51]
    real_value_45_to_49 <- c(real_value_45_to_49, real)
    
    # RU-MIDAS
    ru <- midas_r(mls(interest_rate_ts_window_1, (m_d - h), m_d) ~ 
                    mls(interest_rate_ts_window_1, (m_d - h + 1) + 0:21, m_d) + 
                    mls(cpi_ts_window_1,  2:13, m_m) + 
                    mls(gdp_ts_window_1, 1:4, 1) , 
                  data = list(interest_rate_ts_window_1 = interest_rate_in_sample[[i]],
                              cpi_ts_window_1 = cpi_in_sample[[i]], 
                              gdp_ts_window_1 = gdp_in_sample[[i]]), start = NULL)
    pred_ru <- predict(ru, newdata = list(interest_rate_ts_window_1 = interest_rate_out_of_sample[[i]],
                                          cpi_ts_window_1 = cpi_out_of_sample[[i]], 
                                          gdp_ts_window_1 = gdp_out_of_sample[[i]]))
    ru_pred_45_to_49 <- c(ru_pred_45_to_49, pred_ru[length(pred_ru)])
    
    # RR-MIDAS
    rr <- midas_r(mls(interest_rate_ts_window_1, (m_d - h), m_d) ~ 
                    mls(interest_rate_ts_window_1, (m_d - h + 1) + 0:21, m_d, custom_nealmon) + 
                    mls(cpi_ts_window_1, 2:13, m_m, custom_nealmon) + 
                    mls(gdp_ts_window_1, 1:4, 1, custom_nealmon) , 
                  data = list(interest_rate_ts_window_1 = interest_rate_in_sample[[i]],
                              cpi_ts_window_1 = cpi_in_sample[[i]], 
                              gdp_ts_window_1 = gdp_in_sample[[i]]), 
                  start = list(interest_rate_ts_window_1 = c(1, -0.05),
                               cpi_ts_window_1 = c(1, -0.05),
                               gdp_ts_window_1 = c(1, -0.05)), method = 'BFGS',
                  control = list(maxit = 10000))
    
    pred_rr <- predict(rr, newdata = list(interest_rate_ts_window_1 = interest_rate_out_of_sample[[i]],
                                          cpi_ts_window_1 = cpi_out_of_sample[[i]], 
                                          gdp_ts_window_1 = gdp_out_of_sample[[i]]))
    rr_pred_45_to_49 <- c(rr_pred_45_to_49, pred_rr[length(pred_rr)])
    converge_45_to_49 <- c(converge_45_to_49, rr$convergence)
    
    # 僅使用interest rate所做的interest_rate_only model
    interest_rate_only <- lm(mls(interest_rate_ts_window_1, (m_d - h), m_d) ~ 
                               mls(interest_rate_ts_window_1, (m_d - h + 1) + 0:21, m_d), 
                             data = list(interest_rate_ts_window_1 = interest_rate_in_sample[[i]]))
    pred_interest_rate_only <- predict(interest_rate_only, newdata = list(interest_rate_ts_window_1 = interest_rate_out_of_sample[[i]]))
    interest_rate_only_pred_45_to_49 <- c(interest_rate_only_pred_45_to_49, pred_interest_rate_only[length(pred_interest_rate_only)])
  }
}


# 此為h = 50-66
rr_pred_50_to_66 <- c()
ru_pred_50_to_66 <- c()
interest_rate_only_pred_50_to_66 <- c()
real_value_50_to_66 <- c()
converge_50_to_66 <- c()
for (i in 1:34){
  for (h in 50:66){
    # 真實資料
    real <- mls(interest_rate_out_of_sample[[i]], (m_d - h), m_d)[51]
    real_value_50_to_66 <- c(real_value_50_to_66, real)
    
    # RU-MIDAS
    ru <- midas_r(mls(interest_rate_ts_window_1, (m_d - h), m_d) ~ 
                    mls(interest_rate_ts_window_1, (m_d - h + 1) + 0:21, m_d) + 
                    mls(cpi_ts_window_1, 1:12, m_m) + 
                    mls(gdp_ts_window_1, 1:4, 1) , 
                  data = list(interest_rate_ts_window_1 = interest_rate_in_sample[[i]],
                              cpi_ts_window_1 = cpi_in_sample[[i]], 
                              gdp_ts_window_1 = gdp_in_sample[[i]]), start = NULL)
    pred_ru <- predict(ru, newdata = list(interest_rate_ts_window_1 = interest_rate_out_of_sample[[i]],
                                          cpi_ts_window_1 = cpi_out_of_sample[[i]], 
                                          gdp_ts_window_1 = gdp_out_of_sample[[i]]))
    ru_pred_50_to_66 <- c(ru_pred_50_to_66, pred_ru[length(pred_ru)])
    
    # RR-MIDAS
    rr <- midas_r(mls(interest_rate_ts_window_1, (m_d - h), m_d) ~ 
                    mls(interest_rate_ts_window_1, (m_d - h + 1) + 0:21, m_d, custom_nealmon) + 
                    mls(cpi_ts_window_1, 1:12, m_m, custom_nealmon) + 
                    mls(gdp_ts_window_1, 1:4, 1, custom_nealmon) , 
                  data = list(interest_rate_ts_window_1 = interest_rate_in_sample[[i]],
                              cpi_ts_window_1 = cpi_in_sample[[i]], 
                              gdp_ts_window_1 = gdp_in_sample[[i]]), 
                  start = list(interest_rate_ts_window_1 = c(1, -0.05),
                               cpi_ts_window_1 = c(1, -0.05),
                               gdp_ts_window_1 = c(1, -0.05)), method = 'BFGS',
                  control = list(maxit = 10000))
    
    pred_rr <- predict(rr, newdata = list(interest_rate_ts_window_1 = interest_rate_out_of_sample[[i]],
                                          cpi_ts_window_1 = cpi_out_of_sample[[i]], 
                                          gdp_ts_window_1 = gdp_out_of_sample[[i]]))
    rr_pred_50_to_66 <- c(rr_pred_50_to_66, pred_rr[length(pred_rr)])
    converge_50_to_66 <- c(converge_50_to_66, rr$convergence)
    
    # 僅使用interest rate所做的interest_rate_only model
    interest_rate_only <- lm(mls(interest_rate_ts_window_1, (m_d - h), m_d) ~ 
                               mls(interest_rate_ts_window_1, (m_d - h + 1) + 0:21, m_d), 
                             data = list(interest_rate_ts_window_1 = interest_rate_in_sample[[i]]))
    pred_interest_rate_only <- predict(interest_rate_only, newdata = list(interest_rate_ts_window_1 = interest_rate_out_of_sample[[i]]))
    interest_rate_only_pred_50_to_66 <- c(interest_rate_only_pred_50_to_66, pred_interest_rate_only[length(pred_interest_rate_only)])
  }
}


### 將上述的h= 1-5、h = 6-27、h = 28-44、h = 45-49、h = 50-66的模型RMSE結果，
### 依照順序輸入至各自模型的存放地，為了後續畫圖方便操作
rr_pred <- c()
ru_pred <- c()
interest_rate_only_pred <- c()
real_value <- c()
converge <- c()

for (i in 0:33){
  rr_pred <- c(rr_pred, rr_pred_1_to_5[(5*i+1):(5*i+5)])
  ru_pred <- c(ru_pred, ru_pred_1_to_5[(5*i+1):(5*i+5)])
  interest_rate_only_pred <- c(interest_rate_only_pred, interest_rate_only_pred_1_to_5[(5*i+1):(5*i+5)])
  real_value <- c(real_value, real_value_1_to_5[(5*i+1):(5*i+5)])
  converge <- c(converge, converge_1_to_5[(5*i+1):(5*i+5)])
  
  rr_pred <- c(rr_pred, rr_pred_6_to_27[(22*i+1):(22*i+22)])
  ru_pred <- c(ru_pred, ru_pred_6_to_27[(22*i+1):(22*i+22)])
  interest_rate_only_pred <- c(interest_rate_only_pred, interest_rate_only_pred_6_to_27[(22*i+1):(22*i+22)])
  real_value <- c(real_value, real_value_6_to_27[(22*i+1):(22*i+22)])
  converge <- c(converge, converge_6_to_27[(22*i+1):(22*i+22)])
  
  rr_pred <- c(rr_pred, rr_pred_28_to_44[(17*i+1):(17*i+17)])
  ru_pred <- c(ru_pred, ru_pred_28_to_44[(17*i+1):(17*i+17)])
  interest_rate_only_pred <- c(interest_rate_only_pred, interest_rate_only_pred_28_to_44[(17*i+1):(17*i+17)])
  real_value <- c(real_value, real_value_28_to_44[(17*i+1):(17*i+17)])
  converge <- c(converge, converge_28_to_44[(17*i+1):(17*i+17)])
  
  rr_pred <- c(rr_pred, rr_pred_45_to_49[(5*i+1):(5*i+5)])
  ru_pred <- c(ru_pred, ru_pred_45_to_49[(5*i+1):(5*i+5)])
  interest_rate_only_pred <- c(interest_rate_only_pred, interest_rate_only_pred_45_to_49[(5*i+1):(5*i+5)])
  real_value <- c(real_value, real_value_45_to_49[(5*i+1):(5*i+5)])
  converge <- c(converge, converge_45_to_49[(5*i+1):(5*i+5)])
  
  rr_pred <- c(rr_pred, rr_pred_50_to_66[(17*i+1):(17*i+17)])
  ru_pred <- c(ru_pred, ru_pred_50_to_66[(17*i+1):(17*i+17)])
  interest_rate_only_pred <- c(interest_rate_only_pred, interest_rate_only_pred_50_to_66[(17*i+1):(17*i+17)])
  real_value <- c(real_value, real_value_50_to_66[(17*i+1):(17*i+17)])
  converge <- c(converge, converge_50_to_66[(17*i+1):(17*i+17)])
}

# 確認RR-MIDAS模型全部都收斂了
sum(converge) # 0：皆收斂 



### 線性插值模型
### 將gdp資料，利用線性插值，插值成每日資料(高頻資料)
gdp_copy <- total_gdp_data
# 將第一個gdp值(1999Q1)，利用1998Q4的資料填補
gdp_copy$gdp_log_diff[1] <- diff(log(c(111863, 109755)))
gdp_copy <- gdp_copy[-84, ]
T_gdp <- length(gdp_copy$gdp_log_diff) - 1
gdp_high_freq <- numeric(T_gdp * 66)
gdp_high_freq[1] <- gdp_copy$gdp_log_diff[1]
# 1999Q1~2019Q3
for (t in 1:T_gdp) {
  for (s in 1:66) {
    gdp_high_freq[(t - 1) * 66 + s + 1] <- (1 - s / 66) * gdp_copy$gdp_log_diff[t] + (s / 66) * gdp_copy$gdp_log_diff[t + 1]
  }
}

# GDP資料：1999/01/03-2019/05/01
gdp_high_freq <- gdp_high_freq[c(-1:-2, -5371:-5413)]
length(gdp_high_freq) # 5368



### 將cpi資料，利用線性插值，插值成每日資料(高頻資料)
cpi_copy <- total_cpi_data
# 1999M6~2019M11
cpi_copy <- cpi_copy[c(-1:-5, -252), ]
T_cpi <- length(cpi_copy$cpi_log_diff) - 1
cpi_high_freq <- numeric(T_cpi * 22)
cpi_high_freq[1] <- cpi_copy$cpi_log_diff[1]
for (t in 1:T_cpi) {
  for (s in 1:22) {
    cpi_high_freq[(t - 1) * 22 + s + 1] <- (1 - s / 22) * cpi_copy$cpi_log_diff[t] + (s / 22) * cpi_copy$cpi_log_diff[t + 1]
  }
}

# CPI資料：1999/06/28(h = 20)~2019/10/27(h = 18)
cpi_high_freq <- cpi_high_freq[c(-1:-19, -5388:-5391)] 
length(cpi_high_freq) # 5368


### 由於隨機補值是將一季的資料補成66筆資料，但因為此筆資料（台灣十年期國債資料）
### 選取時間較為特殊（是從1999年9月開始選取的），因此，需要確定9月資料的筆數為何
### 選取十年期國債殖利率資料，從1999/09/01~2019/12/31

# 計算 1999 年 9 月的資料數量
count_sept_1999 <- sum(total_DGS10_data$日期 >= '1999-09-01' & total_DGS10_data$日期 <= '1999-09-30')

# 根據條件選擇起始日期
start_date <- case_when(
  count_sept_1999 == 22 ~ '1999-09-01',
  count_sept_1999 == 21 ~ '1999-08-31',
  count_sept_1999 == 20 ~ '1999-08-30'
)

# 選取符合條件的資料
interest_rate_high_freq <- total_DGS10_data %>%
  filter(日期 >= start_date & 日期 <= '2019-12-31')

interest_rate_high_freq <- interest_rate_high_freq$收市
length(interest_rate_high_freq) #5368



# 將所有資料的22個lags(歷史資料)放到X矩陣
n <- length(interest_rate_high_freq)
m <- 22
X <- matrix(0, nrow = (n - m), ncol = 3 * m)
for (s in m:(n - 1)) {
  X[s - m + 1, 1:m] <- gdp_high_freq[(s - m + 1):s]
  X[s - m + 1, (m + 1):(2 * m)] <- cpi_high_freq[(s - m + 1):s]
  X[s - m + 1, (2 * m + 1):(3 * m)] <- interest_rate_high_freq[(s - m + 1):s]
}

# y是對應到的y值(十年期國債殖利率)：從1999/10/01到2019/12/31間
y <- interest_rate_high_freq[(m+1):n] # 5346

### 線性插值模型開始訓練及預測
# 將資料劃分為31個區間，再將每個區間畫分為in_sample及out_of_sample
LI <- c()
for (i in 1:31) {
  for (h in 1:66){
    split <- m_d * (50 + i - 1)
    in_sample_x <- X[(m_d*(i-1)+h):(split-66+h), 1:(3*m)] 
    out_of_sample_x <- X[(m_d*(i-1)+66+h):(split+h), 1:(3*m)] 
    in_sample_y <- y[(m_d*(i-1)+h):(split-66+h)]
    out_of_sample_y <- y[(m_d*(i-1)+66+h):(split+h)]
    model <- lm(in_sample_y ~ in_sample_x)
    pr <- predict(model, newdata = as.data.frame(out_of_sample_x))
    LI <- c(LI, (out_of_sample_y[3235] - pr[3235]))
  }
}

#############################    注意    #####################################
### 將RR-MIDAS、RU-MIDAS、僅interest rate only的模型的結果輸入至下方的
### result_1中，計算RMSE，以利後續畫圖。
### 注意！！！此dataframe的名稱需要每次做更改：result_1或result_2,...,result_10
### （每次執行完上面全部的程式碼後，都需要將上面的結果輸入至下面的程式碼中，
### 因為十年期國債殖利率資料有隨機補值（前面有進行這樣的處理），所以會有隨機性的問題，
### 因此，為了避免這樣的問題，我們需要隨機補值10次，並將每次的結果儲存起來，
### 所以我們需要每次都將結果的名稱進行更改，並重複10次，最後再計算10次平均的RMSE）
result_1 <- data.frame()
for (i in 1:66){
  indices <- seq(i, length(rr_pred), by = 66)
  result_1 <- rbind(result_1, data.frame(h = i, value = sqrt(mean((rr_pred[indices] - real_value[indices])**2)), 
                                     type = 'rr'))
  result_1 <- rbind(result_1, data.frame(h = i, value = sqrt(mean((ru_pred[indices] - real_value[indices])**2)), 
                                     type = 'ru'))
  result_1 <- rbind(result_1, data.frame(h = i, value = sqrt(mean((interest_rate_only_pred[indices] - real_value[indices])**2)), 
                                     type = 'interest_rate_only'))
}
result_1$type <- factor(result_1$type, levels = c("ru", "rr", "interest_rate_only"))
ggplot(result_1, aes(x = h, y = value, color = type)) +
  geom_point() + 
  geom_line() +
  scale_color_manual(values = c('black', 'red', 'blue')) +  
  scale_x_discrete(limits = as.character(1:66)) +
  labs(y = 'RMSE', 
       title = 'result_1, 初始值為(1, -0.05)')



# 將線性插值模型輸入至result_1中，並計算RMSE
for (i in 1:66){
  indices <- seq(i, length(LI), by = 66)
  result_1 <- rbind(result_1, data.frame(h = i, value = sqrt(mean((LI[indices])**2)), 
                                     type = 'LI'))
}

# 畫出四種model的RMSE
result_1$type <- factor(result_1$type, levels = c("ru", "rr", "interest_rate_only", "LI"))
ggplot(result_1, aes(x = h, y = value, color = type)) +
  geom_point() + 
  geom_line() +
  scale_color_manual(values = c('black', 'red', "blue", "green")) +  
  scale_x_discrete(limits = as.character(1:66)) +
  labs(y = 'RMSE', 
       title = 'result_1, 使用初始值(1,-0.05)估計')


#############################    注意    #####################################
#### 得到10個result_（result_1, result_2, ..., result_10）後
#### 再畫最後一張10個平均的RMSE圖！！！（要得到10個result_，才可以執行這裡的程式碼）
# 將全部10個result結果結合
combined_result <- bind_rows(result_1, result_2, result_3, result_4, result_5, 
                             result_6, result_7, result_8, result_9, result_10) %>%
  group_by(h, type) %>%
  summarise(value = mean(value), .groups = "drop")


# 僅RR-MIDAS、RU-MIDAS、僅stock price only三個模型的10次平均RMSE
ggplot(combined_result %>% filter(type != "LI"), aes(x = h, y = value, color = type)) +
  geom_point() + 
  geom_line() +
  scale_color_manual(values = c('black', 'red', 'blue')) +  # 移除 'green' (因為 'LI' 被移除了)
  scale_x_discrete(limits = as.character(1:66)) +
  labs(y = 'RMSE', 
       title = 'interest_rate, 10個result, 初始值為(1, -0.05)')


# 四個模型的10次平均RMSE
combined_result$type <- factor(combined_result$type, levels = c("ru", "rr", "interest_rate_only", "LI"))
ggplot(combined_result, aes(x = h, y = value, color = type)) +
  geom_point() + 
  geom_line() +
  scale_color_manual(values = c('black', 'red', 'blue', 'green')) +  
  scale_x_discrete(limits = as.character(1:66)) +
  labs(y = 'RMSE', 
       title = 'interest_rate, 10個result, 初始值為(1, -0.05)')
