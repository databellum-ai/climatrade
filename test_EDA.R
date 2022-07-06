
source("10_initialize.R")

library(tidyverse)

df_planetMood <- readRDS("data/df_planetMood.rds")  # Load dataset for analysis
names(df_planetMood)

# https://www.educba.com/arima-model-in-r/
# https://rpubs.com/riazakhan94/arima_with_example

data <- df_planetMood[,c("date","VIX")] %>% arrange(date)
dataTS <- ts(data[,2]+30, start = c(2017,1,1),frequency = 365.25)
plot(dataTS, xlab='Date', ylab = 'VIX')
# Step-2
plot(diff(dataTS),ylab='Differenced VIX')
# Step-3: Carrying Log Transform Data
plot(log10(dataTS),ylab='Log (VIX)')
# Step-4: Difference value
plot(diff(log10(dataTS)),ylab='Differenced Log (VIX)')
# Step-5: Evaluate and iterate
require(forecast)
ARIMAfit = auto.arima(log10(dataTS), approximation=FALSE,trace=FALSE)
summary(ARIMAfit)
# Series: log10(data)
# ARIMA(0,1,1)(1,1,0)[12] Coefficients:
#   ma1 sar1
# -0.5618 -0.6057
# s.e. 0.1177 0.1078
# sigma^2 estimated as 0.000444: log likelihood=142.17
# AIC=-278.34 AICc=-277.91 BIC=-272.11
# Training set error measures:
#   ME RMSE MAE MPE
# Training set 0.000213508 0.01874871 0.01382765 0.009178474
# MAPE MASE ACF1
# Training set 0.5716986 0.2422678 0.0325363
# Step-6 : To examine P and Q values we need to execute acf() and pacf() which is an autocorrelation function.



# ------------------------------------
global_economy %>%
  filter(Code == "EGY") %>%
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Egyptian exports")
fit <- global_economy %>%
  filter(Code == "EGY") %>%
  model(ARIMA(Exports))
report(fit)
fit %>% forecast(h=10) %>%
  autoplot(global_economy) +
  labs(y = "% of GDP", title = "Egyptian exports")
global_economy %>%
  filter(Code == "EGY") %>%
  ACF(Exports) %>%
  autoplot()

df_planetMood_ts %>%
  autoplot(difference(VIX)) +
  labs(y = "VIX", title = "VIX (volatility)")
fit <- df_planetMood_ts %>%
  model(ARIMA(VIX))
report(fit)
fit %>% forecast(h=180) %>%
  autoplot(df_planetMood_ts) +
  labs(y = "VIX", title = "VIX (volatility)")

df_planetMood_ts %>%
  ACF(difference(VIX)) %>%
  autoplot()
# --------------------------------------------------

library(forecast)
library(Metrics)
# data = read.csv('https://raw.githubusercontent.com/jbrownlee/Datasets/master/monthly-car-sales.csv')
# names(data) <- c("date","VIX")
data <- df_planetMood[,c("date","VIX")] %>% mutate(VIX = VIX+30) %>% arrange(date)
n_train <- round(nrow(data) * 0.8, 0)
train <- data[1:n_train,]
test <- data[(n_train+1):nrow(data),] 

model = auto.arima(train[,2])
summary(model)
forecast = predict(model,(nrow(data)-(n_train)))

VIX_predicted[1:n_train] <- data[1:n_train,2]
VIX_predicted[(n_train+1):nrow(data)] <- forecast$pred

result <- cbind(data, VIX_predicted)
result_future <- result[(n_train+1):nrow(data),]
rmse(result_future$VIX,result_future$VIX_predicted[10:20])

plot(ts(result))


