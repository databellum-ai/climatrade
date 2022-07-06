
source("10_initialize.R")

library(tidyverse)

df_planetMood <- readRDS("data/df_planetMood.rds")  # Load dataset for analysis
names(df_planetMood)

# https://www.educba.com/arima-model-in-r/
# https://rpubs.com/riazakhan94/arima_with_example


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
data <- 
  df_planetMood[,c("date","VIX")] %>% 
  mutate(
    VIX = VIX+30, 
    diffVIX = difference(VIX)) %>% 
  arrange(date)

n_train <- round(nrow(data) * 0.8, 0)
train <- data[1:n_train,]
test <- data[(n_train+1):nrow(data),] 

model = auto.arima(train$VIX)
summary(model)
forecast = predict(model,(nrow(data)-(n_train)))

VIX_predicted <- NA
VIX_predicted[1:n_train] <- data[1:n_train,2]
VIX_predicted[(n_train+1):nrow(data)] <- forecast$pred

result <- cbind(data, VIX_predicted)
result_future <- result[(n_train+1):nrow(data),]
rmse(result_future$VIX,result_future$VIX_predicted)

plot(ts(result))

# --------------------------------------------------
data <- df_planetMood[,c("date","VIX")] %>% arrange(date)
data$VIX <- data$VIX + 30
dataTS <- ts(data[,2]+30, start = c(2017,1,1),frequency = 365.25)
plot(dataTS, xlab='Date', ylab = 'VIX')
# Step-2
# plot(diff(dataTS),ylab='Differenced VIX')
# # Step-3: Carrying Log Transform Data
# plot(log10(dataTS),ylab='Log (VIX)')
# Step-4: Difference value
plot(diff(log10(dataTS)),ylab='Differenced Log (VIX)')
# Step-5: Evaluate and iterate
require(forecast)
ARIMAfit = auto.arima(log10(dataTS), approximation=FALSE,trace=FALSE)
summary(ARIMAfit)

# ================================================
set.seed(250)
timeseries=arima.sim(list(order = c(1,1,2), ma=c(0.32,0.47), ar=0.8), n = 50)+20
plot(timeseries)
acf(timeseries)
pacf(timeseries)

## partition into train and test
train_series=timeseries[1:40]
test_series=timeseries[41:50]

## make arima models
arimaModel_1=arima(train_series, order=c(0,1,2))
arimaModel_2=arima(train_series, order=c(1,1,0))
arimaModel_3=arima(train_series, order=c(1,1,2))

## look at the parameters
print(arimaModel_1)
print(arimaModel_2)
print(arimaModel_3)


forecast1=predict(arimaModel_1, 10)
forecast2=predict(arimaModel_2, 10)
forecast3=predict(arimaModel_3, 10)


# ---------------
# https://rpubs.com/riazakhan94/arima_with_example

library(forecast)
library(Metrics)

data <- 
  df_planetMood[,c("date","VIX")] %>% 
  mutate(VIX = VIX+30) %>% 
  arrange(date)

n_train <- round(nrow(data) * 0.8, 0)
train <- data[1:n_train,]
test <- data[(n_train+1):nrow(data),] 
train_series <- train$VIX
test_series <- test$VIX

## make arima models
intervals <- nrow(data) - n_train
# intervals <- 30

# acf(train_series)
# pacf(train_series)
# acf(diff(train_series))
# pacf(diff(train_series))
# auto.arima(train_series)

arimaModel=arima(train_series, order=c(2,1,4))
forecast=predict(arimaModel, intervals)

VIX_predicted <- NA
VIX_predicted[1:n_train] <- data[1:n_train,2]
VIX_predicted[(n_train+1):(n_train+intervals)] <- forecast$pred
result <- cbind(data[1:(n_train+intervals),], VIX_predicted)

plot(ts(result[(n_train+1):(n_train+intervals),]))

result_future <- result[(n_train+1):(n_train+intervals),]
rmse(result_future$VIX,result_future$VIX_predicted)



