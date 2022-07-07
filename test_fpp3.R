#========================================================#
# Forecasting: Principles and Practice (3rd ed)
# Rob J Hyndman and George Athanasopoulos
# 
# https://otexts.com/fpp3/
#========================================================#
  
source("10_initialize.R")

# install.packages("fpp3")
# install.packages("GGally")
# install.packages("fable.prophet")
library(fpp3)
library(GGally)

library(tidyverse)

#========================================================
# Data
#========================================================

# PlanetMood dataset
df_planetMood <- readRDS("data/df_planetMood.rds") %>% arrange(date) %>% mutate(VIX = VIX+30) # Dataset ready for analysis 
names(df_planetMood)
df_planetMood_train <- df_planetMood %>% filter(date <= "2022-05-15")
df_planetMood_ts <- df_planetMood %>% as_tsibble(index = date)  # convert to tsibble (time serie)
df_planetMood_ts_train <- df_planetMood_train %>% as_tsibble(index = date)
futureData_ts <- as_tsibble(df_planetMood %>% filter(date >= "2022-01-01"))


# create a tidy (long) version
df_planetMood_tidy_ts <- df_planetMood %>% gather(key = "variable", value="value", c(-date)) %>% as_tsibble(index = date, key = variable)

# create a lagged dataset
lagsToGenerate <- 3
df_planetMood_ts_lagged <- NULL
lagged_all <- NULL
for (i in c(0:lagsToGenerate)) {
  lagged_all <- rbind(lagged_all, df_planetMood_ts[,-1] %>% lag(n = i) %>% mutate(lag = i))
  # %>% mutate(lag = i) 
  df_planetMood_ts_lagged <- cbind(df_planetMood_ts$date, lagged_all) %>% rename("date" = 1) %>% arrange(date)
}

# Available datasets
df_planetMood
df_planetMood_train
df_planetMood_ts_train
df_planetMood_ts
df_planetMood_tidy_ts
df_planetMood_ts_lagged


# ------------------------------------
# EDA

# Draw Graph n x m
selectedVbles <- names(df_planetMood[c(-1)])
n <- length(selectedVbles)
# we calculate how many rows and column are needed to represent all variables as squeared as possible
r <- c <- round(sqrt(n),0)  # try to use squared matrix for representation of all variables
if (r*c < n) {c <- c+1}  # if not enough, we try one more column
if (r*c < n) {r <- r+1}  # if still not enough, we add one row
par(mfrow=c(r,c), mar=c(5,3,3,3))
for(i in 1:n) {
  matplot(df_planetMood[,selectedVbles][,i], axes=FALSE,
          type=c('l'), col = c('blue'), 
          main = names(df_planetMood[,selectedVbles])[i])
  axis(2) # show y axis
  axis(1, at=seq_along(1:nrow(df_planetMood)),
       labels=df_planetMood$date, las=2)
}


# Function to draw 1:1 comparison in two different axis
chartTwoAxis <- function(x, y1, y2, y1_name, y2_name) {
  par(mfrow=c(1,1))
  par(mar=c(5,5,5,5)+0.1, las=1)
  plot.new()
  plot.window(xlim=range(x), ylim=range(y1))
  lines(x, y1, col="red", pch=19, lwd=1)
  axis(1, col.axis="blue")
  axis(2, col.axis="red")
  box()
  plot.window(xlim=range(x), ylim=range(y2))
  lines(x, y2, col="limegreen", pch=19, lwd=1)
  axis(4, col.axis="limegreen")
  title(paste("1:1 compared history", y1_name, "vs", y2_name), adj=0)
  mtext(y2_name, side = 4, las=3, line=3, col="limegreen")
  mtext(y1_name, side = 2, las=3, line=3, col="red") }
# Call the function to draw 1:1 comparison in two different axis
chartTwoAxis(df_planetMood$date, df_planetMood$VIX, df_planetMood$VVIX, "VIX", "VVIX")


# Pairs correlations in date groups
library(xts)    
selectedVbles <- c("VIX", "VVIX", "Tempo", "NewsTone")
tmpData <- df_planetMood[,c("date", selectedVbles)]
tmpData <- as.xts(tmpData[, -1], order.by = tmpData$date)
group <- NA
firstQuartileDates <- as.Date(as.integer(summary(df_planetMood$date)[2]))
thirdQuartileDates <- as.Date(as.integer(summary(df_planetMood$date)[5]))
group[df_planetMood$date < firstQuartileDates] <- 1
group[df_planetMood$date >= firstQuartileDates & df_planetMood$date <= thirdQuartileDates] <- 2
group[df_planetMood$date > thirdQuartileDates] <- 3
pairs(coredata(tmpData), 
      lower.panel = NULL,
      col = c("red", "blue", "green")[group],
      pch = 18,
      main = "Pairs correlationscolorred by dates")


# Pairs correlations in date groups
# install.packages("tseries")
library(tseries)
yt <- df_planetMood$VIX
xt <- df_planetMood$VVIX 
ccf(yt, xt, lag=90, plot=TRUE, xlim=range(-90,-1))
ccf(yt, xt, lag=90, plot=FALSE, xlim=range(-90,-1))


# Pairs correlations in date groups
# install.packages("astsa")
library(astsa)
VIX <- df_planetMood$VIX
VVIX <- df_planetMood$VVIX
lag2.plot(VVIX, VIX, 
          max.lag = 15, 
          smooth = TRUE, 
          cex=0.2, pch=19, col=5, bgl='transparent', lwl=2, gg=T, box.col=gray(1))


# cross-correlation and distribution
df_planetMood_ts %>%
  GGally::ggpairs(columns = (2:4))


# lag-plot by season
df_planetMood_ts %>% gg_lag(VIX, geom = "point")

# decomposition
dcmp <- df_planetMood_ts %>% model(stl = STL(VIX))
components(dcmp)
components(dcmp) %>%
  as_tsibble() %>%
  autoplot(VIX, colour="gray") +
  geom_line(aes(y=trend), colour = "#D55E00")
components(dcmp) %>% autoplot()
components(dcmp) %>%
  as_tsibble() %>%
  autoplot(VIX, colour="gray") +
  geom_line(aes(y=season_adjust), colour = "#D55E00")

df_planetMood_ts %>%
  model(
    STL(VIX ~ trend(window = 7) +
          season(window = "periodic"),
        robust = TRUE)) %>%
  components() %>%
  autoplot()


# ------------------------------------
# 9.7 ARIMA
# Other ARIMA links:
# https://www.educba.com/arima-model-in-r/
# https://rpubs.com/riazakhan94/arima_with_example
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


# ------------------------------------
# 12.1 STL
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
# 12.2 ARIMA vs STL vs prophet
library(fable.prophet)
# cement <- aus_production %>%
#   filter(year(Quarter) >= 1988)
# train <- cement %>%
#   filter(year(Quarter) <= 2007)
# fit <- train %>%
#   model(
#     arima = ARIMA(Cement),
#     ets = ETS(Cement),
#     prophet = prophet(Cement ~ season(period = 4, order = 2, type = "multiplicative"))
#   )
# fc <- fit %>% forecast(h = "2 years 6 months")
# fc %>% autoplot(cement)
train <- df_planetMood_ts_train
fit <- train %>%
  model(
    arima = ARIMA(VIX),
    ets = ETS(VIX),
    prophet = prophet(VIX ~ season(period = "day", order = 6, type = "multiplicative"))
  )
fc <- fit %>% forecast(h = "14 days")
fc %>% autoplot(df_planetMood_ts[(nrow(df_planetMood_ts)-90):(nrow(df_planetMood_ts)),])
fc %>% accuracy(df_planetMood_ts[(nrow(df_planetMood_ts)-90):(nrow(df_planetMood_ts)),])

# =========
# With ARIMA+Fourier:
fit <- df_planetMood_ts_train %>%
  model(
    ARIMA(VIX ~ PDQ(0, 0, 0) + pdq(d = 0) +
            VVIX + MoonPhase + WkDay + YrWeek +
            fourier(period = "week", K = 3) +
            fourier(period = "month", K = 5) +
            fourier(period = "year", K = 3))
  )
fit %>% gg_tsresiduals()
fit %>% accuracy
fc <- fit %>% forecast(new_data = futureData_ts)
fc %>%
  autoplot(df_planetMood_ts_train %>% tail(10 * 48)) +
  labs(x = "Date", y = "VIX")
# =========
# With prophet:
fit <- df_planetMood_ts_train %>%
  model(prophet(VIX ~ VVIX + MoonPhase + WkDay + YrWeek +
                                   season(period = "week", order = 5) +
                                   season(period = "month", order = 4) +
                                   season(period = "year", order = 3))
  )
fit %>% components() %>% autoplot()
fit %>% gg_tsresiduals()
fit %>% accuracy
fc <- fit %>% forecast(new_data = futureData_ts)
fc %>%
  autoplot(df_planetMood_ts_train %>% tail(10 * 48)) +
  labs(x = "Date", y = "VIX")
# =========
# With both:
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
fit %>% gg_tsresiduals()
fit %>% accuracy
fc <- fit %>% forecast(new_data = futureData_ts)
fc %>%
  autoplot(df_planetMood_ts_train %>% tail(10 * 48)) +
  labs(x = "Date", y = "VIX")


# ------------------------------
# 12.3 VAR
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
  generate(times = 9, h = 30) %>%
  autoplot(.sim) +
  autolayer(sunspots, value) +
  theme(legend.position = "none")



fit <- dataTrain %>%
  model(NNETAR(VIX))
fit %>%
  generate(times = 9, h = 30) %>%
  autoplot(.sim) +
  autolayer(dataTrain, VIX) +
  theme(legend.position = "bottom")



