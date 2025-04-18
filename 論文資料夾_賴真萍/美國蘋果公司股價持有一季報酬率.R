### 載入所需套件
install.packages("devtools")
library(devtools)
install.packages("optimx", dependencies = TRUE)
install_github("mpiktas/midasr", force = TRUE) # 選1
install.packages(c("sandwich", "optimx", "quantreg", "SparseM"))
install.packages("quantmod")
install.packages("readxl")
library(optimx)
library(midasr)
library(readxl)
library(zoo)
library(quantmod)
library(midasr)
library(lubridate)
library(forecast)
library(dplyr)
library(ggplot2)





##### 資料前處理
# 下載並儲存APPLE股價
#getSymbols("AAPL", src = "yahoo", from = "1981-01-01", to = "2018-12-31")
#saveRDS(AAPL, "C:/Users/User/Desktop/AAPL_data.RDS")  # 儲存為 RDS 檔案
#載入apple股價
AAPL <- readRDS("C:/Users/User/Desktop/AAPL_data.RDS") # 讀取固定的 AAPL 數據



### Apple股價資料之敘述統計
#head(AAPL)
#tail(AAPL)
dim(AAPL) # 9581    6
close <- AAPL$AAPL.Close
summary(close)
sqrt(var(as.numeric(close), na.rm = T))


### 隨機補值（補成66個值/1季）
### 因為與預期值（共10032個值），相差451個值，因此計算每個季度總共有多少個值，
### 若是比預期（66個值/1季）少，則隨機抽取幾天，補值（補跟前一天相同的值）。

# 將每筆資料，加上「季度」欄位，以確認是哪個季度的資料
stock_price <- data.frame(DATE = index(AAPL), AAPL.Close = close)
stock_price$YearQuarter <- paste(format(stock_price$DATE, "%Y"), quarters(stock_price$DATE))
row.names(stock_price) <- NULL
head(stock_price)

# 找到所有唯一的季度
unique_quarters <- unique(stock_price$YearQuarter)

# 開始補值
# stock_price_data就是完整的apple stock close price
stock_price_data <- data.frame()

for (quarter in unique_quarters) {
  # 找出當季資料
  current_data <- stock_price[stock_price$YearQuarter == quarter, ]
  current_data$DATE <- as.Date(current_data$DATE)
  
  # 計算該季的缺失值數量
  missing_count <- 66 - nrow(current_data)
  
  # 若是有缺失值
  if (missing_count > 0) {
    # 建造一個整季(三個月)，總共90天的日子
    year <- substr(quarter, 1, 4)
    # 將季度轉為日期，並選取該季度的開始日期與結束日期
    quarter_start <- as.Date(paste0(year, "-", switch(substr(quarter, 6, 7),
                                                      "Q1" = "01-01",
                                                      "Q2" = "04-01",
                                                      "Q3" = "07-01",
                                                      "Q4" = "10-01")))
    quarter_end <- quarter_start + 90 - 1
    # 該季度有股價的第一天
    earliest_date <- min(current_data$DATE)
    # 該季度所有可能的日期，且比earliest_date要晚的日子
    # (為了確保抽到的值，一定有前一個值可以補)
    all_possible_dates <- seq(quarter_start, quarter_end, by = "day")
    
    valid_dates <- all_possible_dates[all_possible_dates >= earliest_date]
    # 找出缺失的日期
    missing_dates <- setdiff(valid_dates, current_data$DATE)
    missing_dates <- as.Date(missing_dates)
    # 隨機選擇想要填補的日期
    random_dates <- sample(missing_dates, size = missing_count, replace = FALSE)
    
    # 將想要填補的日期及股價，填補進來新的row
    new_rows <- data.frame(
      DATE = random_dates,
      AAPL.Close = NA,  
      YearQuarter = quarter
    )
    
    row.names(current_data) <- NULL
    current_data <- rbind(current_data, new_rows)
    
    # 並將所有的row，依照日期先後排序
    current_data <- current_data[order(current_data$DATE), ]
    
    # 使用前一天的股價，填補缺失值NA
    current_data$AAPL.Close <- na.locf(current_data$AAPL.Close)
  }
  
  # 將全部的季度合在一起
  stock_price_data <- rbind(stock_price_data, current_data)
}

dim(stock_price_data) #10032 3
summary(stock_price_data$AAPL.Close)
sqrt(var(stock_price_data$AAPL.Close)) #11.74446
length(stock_price_data$AAPL.Close) #10032

# 確定每一季的資料都是66個
a <- stock_price_data %>%  
  group_by(YearQuarter) %>%
  summarise(Count = n())

which(a$Count != 66) # 確認都是66筆資料/1季

### monthly data：cpi資料的資料處理
# 將資料拆分為兩筆資料，分別進行處理：
# 都是選取資料最初剛發布的版本資料
# 第一筆：1981.01-1998.10
# 第二筆：1998.11-2018.12（需特殊處理：需要取得第一個非NA的數值）
cpi_data <- read_excel('cpi_data.xlsx')
row_1 <- cpi_data[cpi_data$DATE >= "1981:01" & cpi_data$DATE <= "1998:10", 1:2]
row_2 <- cpi_data[cpi_data$DATE >= "1998:11" & cpi_data$DATE <= "2018:12", ]

# 對row2進行處理，取得第一個非NA的數值，即取得cpi資料剛發布的數值
start_col <- which(names(cpi_data) == "PCPI98M11")
end_col <- which(names(cpi_data) == "PCPI19M1")

CPI_filtered <- row_2[, c(1, start_col:end_col)]
CPI_filtered[CPI_filtered == "#N/A"] <- NA

# 將每一個row的NA剔除，並返回第一個非NA值（就是那一個月的vintages data） 
fun <- function(row) {
  first_value <- na.omit(row)[1]
  return(first_value)
}
first_non_na <- apply(CPI_filtered[, -1], 1, fun)
colnames(row_1) <- c("DATE", "cpi")

# 將兩筆資料合併，形成完整的cpi資料集
cpi <- rbind(row_1, data.frame(DATE = CPI_filtered$DATE, cpi = first_non_na))

# 確認資料的summary
cpi$cpi <- as.numeric(cpi$cpi)
dim(cpi) # 456   2
summary(cpi$cpi)
sqrt(var(cpi$cpi)) # 48.29366

### quarterly data：gdp資料處理
# 與上述處理CPI資料相同的方法，取得GDP資料最初剛發布的版本資料
gdp_data <- read_excel('gdp_data.xlsx')
gdp <- gdp_data[gdp_data$DATE >= "1981:Q1" & gdp_data$DATE <= "2018:Q4", ]

# 將每一個row的NA剔除，並返回第一個非NA值 
gdp[gdp == "#N/A"] <- NA

fun <- function(row) {
  first_value <- na.omit(row)[1]
  return(first_value)
}

first_non_na <- apply(gdp[, -1], 1, fun)

# 形成完整的gdp資料集
gdp <- data.frame(DATE = gdp$DATE, gdp = first_non_na)

# 確認資料的summary
dim(gdp) # 152   2
gdp$gdp <- as.numeric(gdp$gdp)
summary(gdp$gdp)
sqrt(var(gdp$gdp)) # 4990.068



### 將三筆資料進行資料前處理
# GDP取一階diff：
gdp$gdp_log_diff <- c(NA, diff(log(gdp$gdp)))
# CPI取percent change：
cpi <- cpi %>%
  mutate(percent_change = (cpi - lag(cpi)) / lag(cpi) * 100)
# APPLE收盤價報酬率：
stock_price_data <- stock_price_data %>%
  mutate(stock_return = (AAPL.Close - lag(AAPL.Close, 66)) / lag(AAPL.Close, 66))



### 將這三個資料，轉換成time series模式，起始為1981年Q1
# gdp_ts：1981Q1~2018Q4
gdp_ts <- ts(c(as.numeric(gdp$gdp_log_diff)), start=c(1981, 1), frequency = 4) 
# cpi_ts：1981M1~2018M12
cpi_ts <- ts(c(as.numeric(cpi$percent_change)), start=c(1981, 1), frequency = 12)
# stock_price_ts：1981/01/01~2018/12/31
stock_price_ts <- ts(c(as.numeric(stock_price_data$stock_return)), start=c(1981, 1), frequency = 22*12)



### rolling forcast window
### 創造gdp的 rolling forcast window
start_time <- c(1981, 1) # 1981Q1
end_time <- c(2018, 4)   # 2018Q4

# 生成每一季的時間點
time_points <- seq(from = start_time[1] + (start_time[2]-1) / 4,
                   to = end_time[1] + (end_time[2]-1) / 4,
                   by = 0.25)

# 切分每一個in_sample及out_of_sample的rolling forecast window
gdp_in_sample <- list()

for (i in seq_len(92)) {
  gdp_in_sample[[i]] <- window(gdp_ts, start = time_points[i], end = time_points[i + 59])
}


gdp_out_of_sample <- list()

for (i in seq_len(92)) {
  gdp_out_of_sample[[i]] <- window(gdp_ts, start = time_points[i], end = time_points[i + 60])
}

gdp_in_sample
gdp_out_of_sample



### 創造cpi的 rolling forcast window
start_time <- c(1981, 1) # 1981年1月
end_time <- c(2018, 12) # 2018年12月

# 生成每個月的時間點
time_points <- seq(from = start_time[1] + (start_time[2] - 1) / 12,
                   to = end_time[1] + (end_time[2] - 1) / 12,
                   by = 1 / 12) 


cpi_in_sample <- list()

for (i in seq_len(92)) {
  cpi_in_sample[[i]] <- window(cpi_ts, start = time_points[1+3*(i-1)], end = time_points[1+3*(i-1)+(60*3-1)])
}

cpi_out_of_sample <- list()

for (i in seq_len(92)) {
  cpi_out_of_sample[[i]] <- window(cpi_ts, start = time_points[1+3*(i-1)], end = time_points[1+3*(i-1)+(61*3-1)])
}

cpi_in_sample
cpi_out_of_sample



### 創造stock price的 rolling forcast window
start_time <- c(1981, 1)  # 1981年1月1日
end_time <- c(2018, 12*22)  # 2018年12月31日

# 生成每個月的時間點
time_points <- seq(from = start_time[1] + (start_time[2] - 1) / (66*4),
                   to = end_time[1] + (end_time[2] - 1) / (66*4),
                   by = 1 / (66*4)) 


stock_price_in_sample <- list()

for (i in seq_len(92)) {
  stock_price_in_sample[[i]] <- window(stock_price_ts, start = time_points[1+66*(i-1)], end = time_points[1+66*(i-1)+(66*60-1)])
}

stock_price_out_of_sample <- list()

for (i in seq_len(92)) {
  stock_price_out_of_sample[[i]] <- window(stock_price_ts, start = time_points[1+66*(i-1)], end = time_points[1+66*(i-1)+(66*61-1)])
}

stock_price_in_sample
stock_price_out_of_sample

### 跑RR-MIDAS、RU-MIDAS、僅stock price only預測模型，
### 這91個（因為不取用第一季，所以92-1=91）window中，每個h(h = 1-66)的RMSE
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


### 因為三筆資料更新時間不同，所以將資料分成h= 1-22及h = 23-66做訓練及預測結果
# 此為h = 1-22
rr_pred_1_to_22 <- c()
ru_pred_1_to_22 <- c()
stock_price_only_pred_1_to_22 <- c()
real_value_1_to_22 <- c()
converge_1_to_22 <- c()
for (i in 2:92){ # 因為從1981Q2才開始取用資料，所以rolling window是從2~92
  for (h in 1:22){
    # 真實資料
    real <- mls(stock_price_out_of_sample[[i]], (m_d - h), m_d)[61]
    real_value_1_to_22 <- c(real_value_1_to_22, real)
    
    # RU-MIDAS
    ru <- midas_r(mls(stock_price_ts_window_1, (m_d - h), m_d) ~ 
                    mls(stock_price_ts_window_1, (m_d - h + 1) + 0:21, m_d) + 
                    mls(cpi_ts_window_1, 4:15, m_m) + 
                    mls(gdp_ts_window_1, 2:5, 1) , 
                  data = list(stock_price_ts_window_1 = stock_price_in_sample[[i]],
                              cpi_ts_window_1 = cpi_in_sample[[i]], 
                              gdp_ts_window_1 = gdp_in_sample[[i]]), start = NULL,
                  control = list(maxit = 10000))
    pred_ru <- predict(ru, newdata = list(stock_price_ts_window_1 = stock_price_out_of_sample[[i]],
                                          cpi_ts_window_1 = cpi_out_of_sample[[i]], 
                                          gdp_ts_window_1 = gdp_out_of_sample[[i]]))
    ru_pred_1_to_22 <- c(ru_pred_1_to_22, pred_ru[length(pred_ru)])
    
    # RR-MIDAS
    rr <- midas_r(mls(stock_price_ts_window_1, (m_d - h), m_d) ~ 
                    mls(stock_price_ts_window_1, (m_d - h + 1) + 0:21, m_d, custom_nealmon) + 
                    mls(cpi_ts_window_1, 4:15, m_m, custom_nealmon) + 
                    mls(gdp_ts_window_1, 2:5, 1, custom_nealmon), 
                  data = list(stock_price_ts_window_1 = stock_price_in_sample[[i]],
                              cpi_ts_window_1 = cpi_in_sample[[i]], 
                              gdp_ts_window_1 = gdp_in_sample[[i]]), 
                  start = list(stock_price_ts_window_1 = c(1, -0.05),
                               cpi_ts_window_1 = c(1, -0.05),
                               gdp_ts_window_1 = c(1, -0.05)), 
                  method = 'BFGS',
                  control = list(maxit = 10000))
    
    pred_rr <- predict(rr, newdata = list(stock_price_ts_window_1 = stock_price_out_of_sample[[i]],
                                          cpi_ts_window_1 = cpi_out_of_sample[[i]], 
                                          gdp_ts_window_1 = gdp_out_of_sample[[i]]))
    rr_pred_1_to_22 <- c(rr_pred_1_to_22, pred_rr[length(pred_rr)])
    converge_1_to_22 <- c(converge_1_to_22, rr$convergence)
    
    # 僅用stock_price lags所做的stock_price_only model
    stock_price_only <- lm(mls(stock_price_ts_window_1, (m_d - h), m_d) ~ 
                                  mls(stock_price_ts_window_1, (m_d - h + 1) + 0:21, m_d), 
                                data = list(stock_price_ts_window_1 = stock_price_in_sample[[i]]))
    pred_stock_price_only <- predict(stock_price_only, newdata = list(stock_price_ts_window_1 = stock_price_out_of_sample[[i]]))
    stock_price_only_pred_1_to_22 <- c(stock_price_only_pred_1_to_22, pred_stock_price_only[length(pred_stock_price_only)])
  }
}



# 此為h = 23-66
rr_pred_23_to_66 <- c()
ru_pred_23_to_66 <- c()
stock_price_only_pred_23_to_66 <- c()
real_value_23_to_66 <- c()
converge_23_to_66 <- c()
for (i in 2:92){
  for (h in 23:66){
    # 真實資料
    real <- mls(stock_price_out_of_sample[[i]], (m_d - h), m_d)[61]
    real_value_23_to_66 <- c(real_value_23_to_66, real)
    
    # RU-MIDAS
    ru <- midas_r(mls(stock_price_ts_window_1, (m_d - h), m_d) ~ 
                    mls(stock_price_ts_window_1, (m_d - h + 1) + 0:21, m_d) + 
                    mls(cpi_ts_window_1, (m_m - floor(h/22.5) + 1) + 0:11, m_m) + 
                    mls(gdp_ts_window_1, 1:4, 1) , 
                  data = list(stock_price_ts_window_1 = stock_price_in_sample[[i]],
                              cpi_ts_window_1 = cpi_in_sample[[i]], 
                              gdp_ts_window_1 = gdp_in_sample[[i]]), start = NULL,
                  control = list(maxit = 10000))
    pred_ru <- predict(ru, newdata = list(stock_price_ts_window_1 = stock_price_out_of_sample[[i]],
                                          cpi_ts_window_1 = cpi_out_of_sample[[i]], 
                                          gdp_ts_window_1 = gdp_out_of_sample[[i]]))
    ru_pred_23_to_66 <- c(ru_pred_23_to_66, pred_ru[length(pred_ru)])
    
    # RR-MIDAS
    rr <- midas_r(mls(stock_price_ts_window_1, (m_d - h), m_d) ~ 
                    mls(stock_price_ts_window_1, (m_d - h + 1) + 0:21, m_d, custom_nealmon) + 
                    mls(cpi_ts_window_1, (m_m - floor(h/22.5) + 1) + 0:11, m_m, custom_nealmon) + 
                    mls(gdp_ts_window_1, 1:4, 1, custom_nealmon), 
                  data = list(stock_price_ts_window_1 = stock_price_in_sample[[i]],
                              cpi_ts_window_1 = cpi_in_sample[[i]], 
                              gdp_ts_window_1 = gdp_in_sample[[i]]), 
                  start = list(stock_price_ts_window_1 = c(1, -0.05),
                               cpi_ts_window_1 = c(1, -0.05),
                               gdp_ts_window_1 = c(1, -0.05)), 
                  method = 'BFGS',
                  control = list(maxit = 10000))
    
    pred_rr <- predict(rr, newdata = list(stock_price_ts_window_1 = stock_price_out_of_sample[[i]],
                                          cpi_ts_window_1 = cpi_out_of_sample[[i]], 
                                          gdp_ts_window_1 = gdp_out_of_sample[[i]]))
    rr_pred_23_to_66 <- c(rr_pred_23_to_66, pred_rr[length(pred_rr)])
    converge_23_to_66 <- c(converge_23_to_66, rr$convergence)
    
    # 僅用stock_price lags所做的stock_price_only model
    stock_price_only <- lm(mls(stock_price_ts_window_1, (m_d - h), m_d) ~ 
                                  mls(stock_price_ts_window_1, (m_d - h + 1) + 0:21, m_d), 
                                data = list(stock_price_ts_window_1 = stock_price_in_sample[[i]]))
    pred_stock_price_only <- predict(stock_price_only, newdata = list(stock_price_ts_window_1 = stock_price_out_of_sample[[i]]))
    stock_price_only_pred_23_to_66 <- c(stock_price_only_pred_23_to_66, pred_stock_price_only[length(pred_stock_price_only)])
  }
}


### 將上述的h = 1-22及h = 23-66的模型RMSE結果，依照順序輸入至各自模型的存放地
### 為了後續畫圖方便操作
rr_pred <- c()
ru_pred <- c()
stock_price_only_pred <- c()
real_value <- c()
converge <- c()

for (i in 0:90){
  rr_pred <- c(rr_pred, rr_pred_1_to_22[(22*i+1):(22*i+22)])
  ru_pred <- c(ru_pred, ru_pred_1_to_22[(22*i+1):(22*i+22)])
  stock_price_only_pred <- c(stock_price_only_pred, stock_price_only_pred_1_to_22[(22*i+1):(22*i+22)])
  real_value <- c(real_value, real_value_1_to_22[(22*i+1):(22*i+22)])
  converge <- c(converge, converge_1_to_22[(22*i+1):(22*i+22)])
  
  rr_pred <- c(rr_pred, rr_pred_23_to_66[(44*i+1):(44*i+44)])
  ru_pred <- c(ru_pred, ru_pred_23_to_66[(44*i+1):(44*i+44)])
  stock_price_only_pred <- c(stock_price_only_pred, stock_price_only_pred_23_to_66[(44*i+1):(44*i+44)])
  real_value <- c(real_value, real_value_23_to_66[(44*i+1):(44*i+44)])
  converge <- c(converge, converge_23_to_66[(44*i+1):(44*i+44)])
}

# 確認RR-MIDAS模型全部都收斂了
sum(converge) # 0：皆收斂


### 線性插值模型

### 將gdp資料，利用線性插值，插值成每日資料(高頻資料)
gdp_copy <- gdp
# 將第一個gdp值(1981Q1)，利用1980Q4的資料填補，就可以得到1981Q1的gdp_log_diff
gdp_copy$gdp_log_diff[1] <- diff(log(c(1490.1, 1509.2)))
# 從1981Q2-2018Q3
gdp_copy <- gdp_copy[c(-1, -152), ]
T_gdp <- length(gdp_copy$gdp_log_diff) - 1
gdp_high_freq <- numeric(T_gdp * 66)
gdp_high_freq[1] <- gdp_copy$gdp_log_diff[1]
# 從1981Q2-2018Q3
for (t in 1:T_gdp) {
  for (s in 1:66) {
    gdp_high_freq[(t - 1) * 66 + s + 1] <- (1 - s / 66) * gdp_copy$gdp_log_diff[t] + (s / 66) * gdp_copy$gdp_log_diff[t + 1]
  }
}
# GDP資料：從1981/05/03-2018/06/01
gdp_high_freq <- gdp_high_freq[c(-1:-24, -9815:-9835)]
length(gdp_high_freq) #9790




### 將cpi資料，利用線性插值，插值成每日資料(高頻資料)
cpi_copy <- cpi
# 從1981M9-2018M10
cpi_copy <- cpi_copy[c(-1:-8, -455:-456), ]
T_cpi <- length(cpi_copy$percent_change) - 1
cpi_high_freq <- numeric(T_cpi * 22)
cpi_high_freq[1] <- cpi_copy$percent_change[1]
for (t in 1:T_cpi) {
  for (s in 1:22) {
    cpi_high_freq[(t - 1) * 22 + s + 1] <- (1 - s / 22) * cpi_copy$percent_change[t] + (s / 22) * cpi_copy$percent_change[t + 1]
  }
}
# CPI資料：從1981/09/03-2018/10/01
cpi_high_freq <- cpi_high_freq[c(-1, -2)]
cpi_high_freq <- c(cpi_high_freq, tail(cpi_copy$percent_change, 1))
length(cpi_high_freq) #9790



# APPLE股價收盤價報酬率資料：從1981/12/01-2018/12/31
stock_price_data_high_freq <- stock_price_data[which(stock_price_data$DATE >= '1981-12-01' & stock_price_data$DATE <= '2018-12-31'), 'stock_return']
length(stock_price_data_high_freq) #9790



# 將所有資料的22個lags(歷史資料)放到X矩陣
n <- length(stock_price_data_high_freq)
m <- 22
X <- matrix(0, nrow = (n - m), ncol = 3 * m)
for (s in m:(n - 1)) {
  X[s - m + 1, 1:m] <- gdp_high_freq[(s - m + 1):s]
  X[s - m + 1, (m + 1):(2 * m)] <- cpi_high_freq[(s - m + 1):s]
  X[s - m + 1, (2 * m + 1):(3 * m)] <- stock_price_data_high_freq[(s - m + 1):s]
}

# y是對應到的y值(APPLE股價報酬率)：從1982/01/01到2018/12/31間
y <- stock_price_data_high_freq[(m+1):n] # 9768(148季)


### 線性插值模型開始訓練及預測
# 將資料劃分為88個區間，再將每個區間畫分為in_sample及out_of_sample
LI <- c()
for (i in 1:88) {
  for (h in 1:66){
    split <- m_d * (60 + i - 1)
    in_sample_x <- X[(m_d*(i-1)+h):(split-66+h), 1:(3*m)] 
    out_of_sample_x <- X[(m_d*(i-1)+66+h):(split+h), 1:(3*m)] 
    in_sample_y <- y[(m_d*(i-1)+h):(split-66+h)]
    out_of_sample_y <- y[(m_d*(i-1)+66+h):(split+h)]
    model <- lm(in_sample_y ~ in_sample_x)
    pr <- predict(model, newdata = as.data.frame(out_of_sample_x))
    LI <- c(LI, (out_of_sample_y[3895] - pr[3895]))
  }
}

#############################    注意    #####################################
### 將RR-MIDAS、RU-MIDAS、僅stock price only的模型的結果輸入至下方的
### apple_result_1中，計算RMSE，以利後續畫圖。
### 注意！！！此dataframe的名稱需要每次做更改：apple_result_1或apple_result_2,...,apple_result_10
### （每次執行完上面全部的程式碼後，都需要將上面的結果輸入至下面的程式碼中，
### 因為APPLE資料有隨機補值（前面有進行這樣的處理），所以會有隨機性的問題，
### 因此，為了避免這樣的問題，我們需要隨機補值10次，並將每次的結果儲存起來，
### 所以我們需要每次都將結果的名稱進行更改，並重複10次，最後再計算10次平均的RMSE）
apple_result_1 <- data.frame()
for (i in 1:66){
  indices <- seq(i, length(rr_pred), by = 66)
  apple_result_1 <- rbind(apple_result_1, data.frame(h = i, value = sqrt(mean((rr_pred[indices] - real_value[indices])**2)), 
                                                     type = 'rr'))
  apple_result_1 <- rbind(apple_result_1, data.frame(h = i, value = sqrt(mean((ru_pred[indices] - real_value[indices])**2)), 
                                                     type = 'ru'))
  apple_result_1 <- rbind(apple_result_1, data.frame(h = i, value = sqrt(mean((stock_price_only_pred[indices] - real_value[indices])**2)), 
                                                     type = 'stock_price_only'))
}
apple_result_1$type <- factor(apple_result_1$type, levels = c("ru", "rr", "stock_price_only"))
ggplot(apple_result_1, aes(x = h, y = value, color = type)) +
  geom_point() + 
  geom_line() +
  scale_color_manual(values = c('black', 'red', 'blue')) +  
  scale_x_discrete(limits = as.character(1:66)) +
  labs(y = 'RMSE', 
       title = 'apple_result_1, 使用初始值(1,-0.05)估計')


# 將線性插值模型輸入至apple_result_1中，並計算RMSE
for (i in 1:66){
  indices <- seq(i, length(LI), by = 66)
  apple_result_1 <- rbind(apple_result_1, data.frame(h = i, value = sqrt(mean((LI[indices])**2)), 
                                     type = 'LI'))
}


# 畫出四種model的RMSE
apple_result_1$type <- factor(apple_result_1$type, levels = c("ru", "rr", "stock_price_only", "LI"))
ggplot(apple_result_1, aes(x = h, y = value, color = type)) +
  geom_point() + 
  geom_line() +
  scale_color_manual(values = c('black', 'red', "blue", "green")) +  
  scale_x_discrete(limits = as.character(1:66)) +
  labs(y = 'RMSE', 
       title = 'apple_result_1, 使用初始值(1,-0.05)估計')


#############################    注意    #####################################
#### 得到10個apple_result_（apple_result_1, apple_result_2, ..., apple_result_10）後
#### 再畫最後一張10個平均的RMSE圖！！！（要得到10個apple_result_，才可以執行這裡的程式碼）
# 將全部10個result結果結合
combined_result <- bind_rows(apple_result_1, apple_result_2, apple_result_3, apple_result_4, apple_result_5,
                             apple_result_6, apple_result_7, apple_result_8, apple_result_9, apple_result_10) %>%
  group_by(h, type) %>%
  summarise(value = mean(value), .groups = "drop")

combined_result$type <- factor(combined_result$type, levels = c("ru", "rr", "stock_price_only", "LI"))


# 僅RR-MIDAS、RU-MIDAS、僅stock price only三個模型的10次平均RMSE
ggplot(combined_result %>% filter(type != "LI"), aes(x = h, y = value, color = type)) +
  geom_point() + 
  geom_line() +
  scale_color_manual(values = c('black', 'red', 'blue')) +  # 移除 'green' (因為 'LI' 被移除了)
  scale_x_discrete(limits = as.character(1:66)) +
  labs(y = 'RMSE', 
       title = 'apple, 10個result, 初始值為(1, -0.05)')


# 四個模型的10次平均RMSE
ggplot(combined_result, aes(x = h, y = value, color = type)) +
  geom_point() + 
  geom_line() +
  scale_color_manual(values = c('black', 'red', 'blue', 'green')) +  
  scale_x_discrete(limits = as.character(1:66)) +
  labs(y = 'RMSE', 
       title = 'apple, 10個result, 初始值為(1, -0.05)')



