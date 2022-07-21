
source("project1_main/10_initialize.R")

# install.packages("fpp3")
# install.packages("GGally")
library(fpp3)
library(GGally)

library(tidyverse)

df_planetMood <- readRDS("data/df_planetMood.rds")  # Load dataset for analysis
names(df_planetMood)
dataTest <- as_tsibble(df_planetMood[,c("date","VIX")], index = "date") %>% arrange(date) %>% mutate(VIX = VIX+30) %>% 
  filter(date >= "2017-01-01")

# https://www.educba.com/arima-model-in-r/
# https://rpubs.com/riazakhan94/arima_with_example
# https://otexts.com/fpp3

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


# ============================
# 8.3

fit <- dataTest %>%
  model(
    additive = ETS(VIX ~ error("A") + trend("A") +
                     season("A")),
    multiplicative = ETS(VIX ~ error("M") + trend("A") +
                           season("M"))
  )
fc <- fit %>% forecast(h = "2 weeks")
fc %>%
  autoplot(dataTest, level = NULL) +
  labs(title="Australian domestic tourism",
       y="Overnight trips (millions)") +
  guides(colour = guide_legend(title = "Forecast"))

# 8.6 COMPONENTS
components(fit) %>%
  autoplot() +
  labs(title = "ETS(M,N,A) components")

# ----------
sth_cross_ped <- pedestrian %>%
  filter(Date >= "2016-07-01",
         Sensor == "Southern Cross Station") %>%
  index_by(Date) %>%
  summarise(Count = sum(Count)/1000)
sth_cross_ped %>%
  filter(Date <= "2016-07-31") %>%
  model(
    hw = ETS(Count ~ error("M") + trend("Ad") + season("M"))
  ) %>%
  forecast(h = "2 weeks") %>%
  autoplot(sth_cross_ped %>% filter(Date <= "2016-08-14")) +
  labs(title = "Daily traffic: Southern Cross",
       y="Pedestrians ('000)")

sth_cross_ped <- dataTest
sth_cross_ped %>%
  model(hw = ETS(VIX ~ error("M") + trend("Ad") + season("M"))) %>%
  forecast(h = "2 weeks") %>%
  autoplot(sth_cross_ped) +
  labs(title = "Daily traffic: Southern Cross",
       y="Pedestrians ('000)")


# ===========================================
# 12.1


bank_calls %>%
  fill_gaps() %>%
  autoplot(Calls) +
  labs(y = "Calls",
       title = "Five-minute call volume to bank")


calls <- bank_calls %>%
  mutate(t = row_number()) %>%
  update_tsibble(index = t, regular = TRUE)
calls %>%
  model(
    STL(sqrt(Calls) ~ season(period = 169) +
          season(period = 5*169),
        robust = TRUE)
  ) %>%
  components() %>%
  autoplot() + labs(x = "Observation")


# ------------------------------------
# 12.2

library(fable.prophet)
cement <- aus_production %>%
  filter(year(Quarter) >= 1988)
train <- cement %>%
  filter(year(Quarter) <= 2007)
fit <- train %>%
  model(
    arima = ARIMA(Cement),
    ets = ETS(Cement),
    prophet = prophet(Cement ~ season(period = 4, order = 2,
                                      type = "multiplicative"))
  )

fc <- fit %>% forecast(h = "2 years 6 months")
fc %>% autoplot(cement)





cement <- dataTest %>% filter(date >= "2022-01-01")
train <- cement %>% filter(date <= "2022-06-01")
fit <- train %>%
  model(
    arima = ARIMA(VIX),
    ets = ETS(VIX),
    prophet = prophet(VIX ~ season(period = "day", order = 6,
                                      type = "multiplicative"))
  )
fc <- fit %>% forecast(h = "14 days")
fc %>% autoplot(cement)
fc %>% accuracy(cement)


# ------------------------------------------------------------------
# 12.2
# DHR (dynamic harmonic regression) model
dataTrain <- as_tsibble(df_planetMood[,c("date","VIX","VVIX", "MoonPhase", "WkDay", "YrWeek")], index = "date") %>% arrange(date) %>% mutate(VIX = VIX+30) %>% filter(date >= "2017-01-01")
# create a lagged dataset
# lagsToGenerate <- 3
# df_planetMood_ts_lagged <- NULL
# lagged_all <- NULL
# for (i in c(0:lagsToGenerate)) {
#   lagged_all <- rbind(lagged_all, df_planetMood_ts[,-1] %>% lag(n = i) %>% mutate(lag = i))
#   # %>% mutate(lag = i) 
#   df_planetMood_ts_lagged <- cbind(df_planetMood_ts$date, lagged_all) %>% rename("date" = 1) %>% arrange(date)
# }
# df_planetMood_ts_lagged
# new data for prediction
futureData <- as_tsibble(dataTrain %>% filter(date >= "2022-01-01"))

# =========
# =========
# With ARIMA+Fourier:
fit <- dataTrain %>%
  model(
    ARIMA(VIX ~ PDQ(0, 0, 0) + pdq(d = 0) +
            VVIX + MoonPhase + WkDay + YrWeek +
            fourier(period = "week", K = 3) +
            fourier(period = "month", K = 5) +
            fourier(period = "year", K = 3))
  )
# =========
# =========
# With Prophet:
fit <- dataTrain %>%
  model(
    modelo_DHR_ARIMA = ARIMA(VIX ~ PDQ(0, 0, 0) + pdq(d = 0) +
                               VVIX + MoonPhase + WkDay + YrWeek +
                               fourier(period = "week", K = 3) +
                               fourier(period = "month", K = 5) +
                               fourier(period = "year", K = 3)),
    modelo_DHR_prophet = prophet(VIX ~ VVIX + MoonPhase + WkDay + YrWeek +
                                   season(period = "week", order = 5) +
                                   season(period = "month", order = 4) +
                                   season(period = "year", order = 3))
  )
# =========
# =========
fit %>% components() %>% autoplot()
fit %>% gg_tsresiduals()
fit %>% accuracy

fc <- fit %>% forecast(new_data = futureData)
fc %>%
  autoplot(dataTrain %>% tail(10 * 48)) +
  labs(x = "Date", y = "VIX")



# ------------------------------
# 12.3 (VAR)
fit <- us_change %>%
  model(
    aicc = VAR(vars(Consumption, Income)),
    bic = VAR(vars(Consumption, Income), ic = "bic")
  )
fit
glance(fit)
fit %>%
  augment() %>%
  ACF(.innov) %>%
  autoplot()
fit %>%
  select(aicc) %>%
  forecast() %>%
  autoplot(us_change %>% filter(year(Quarter) > 2010))


fit <- (dataTrain %>% filter(date <= "2022-02-01")) %>%
  model(
    aicc = VAR(vars(VIX, VVIX)),
    bic = VAR(vars(VIX, VVIX), ic = "bic")
  )
fit
glance(fit)
fit %>%
  augment() %>%
  ACF(.innov) %>%
  autoplot()
fit %>%
  select(aicc) %>%
  forecast(h = 90) %>%
  autoplot(dataTrain %>% filter(date > "2017-01-01"))


# ------------------------------
# 12.4 NEURAL NETWORKS
sunspots <- sunspot.year %>% as_tsibble()
fit <- sunspots %>%
  model(NNETAR(sqrt(value)))
fit %>%
  forecast(h = 30) %>%
  autoplot(sunspots) +
  labs(x = "Year", y = "Counts", title = "Yearly sunspots")

fit %>%
  generate(times = 9, h = 30) %>%
  autoplot(.sim) +
  autolayer(sunspots, value) +
  theme(legend.position = "none")




