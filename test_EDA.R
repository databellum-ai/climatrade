
source("10_initialize.R")

library(tidyverse)

df_planetMood <- readRDS("data/df_planetMood.rds")  # Load dataset for analysis
names(df_planetMood)

# https://www.educba.com/arima-model-in-r/
# https://rpubs.com/riazakhan94/arima_with_example

data <- df_planetMood[,c("date","VIX")] %>% arrange(date)
data <- ts(data[,2]+30, start = c(2017,1,1),frequency = 365.25)
plot(data, xlab='Date', ylab = 'VIX')
# Step-2
plot(diff(data),ylab='Differenced VIX')
# Step-3: Carrying Log Transform Data
plot(log10(data),ylab='Log (VIX)')
# Step-4: Difference value
plot(diff(log10(data)),ylab='Differenced Log (VIX)')
# Step-5: Evaluate and iterate
require(forecast)
ARIMAfit = auto.arima(log10(data), approximation=FALSE,trace=FALSE)
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