# JES: !usar regresión para optimizar criterio BUY/SELL en "accuracies_all" (previamente hacer cálculo masivo >= 100 fechas)
# JES: !en NN+xReg: usar log() y scale() para refinar
# JES: refinar más vblesPlanetMood (movingAverage/diff/log/smooth)
# JES: en NN+xReg:probar VAR (Haydn + Tajendra) para forecast de regressors (*_n) (actuales y vblesPlanetMood)
# JES: en NN+xReg: jugar con más parámetros de nnetar y de forecast
# JES: hyperparameters: ¿"frequency" <> 7?; ¿StrongThrshold = 1%?
# JES: crear shinnyApp


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# INITIALIZATION
# -Environment initialization. Load all packages required
# -Data scope to retrieve 
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

# ===============
# CLEAN ENVIRONMENT
# ---------------
rm(list = ls())  # clear all environment variables
graphics.off()  # clear all graphs

# ===============
# PACKAGES REQUIRED
# ---------------
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(fpp3)) install.packages("fpp3", repos = "http://cran.us.r-project.org")
if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")
if(!require(forecast)) install.packages("forecast", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(fpp3)
library(GGally)
library(forecast)

#========================================================
# FUNCTIONS
#========================================================
# function calculate "PlanetMood" Accuracy for our prediction
calculateAccuracyDataframe_pm <- function(VIX_forecasted) {
  closingRef <- df_planetMood_1 %>% filter(date == lastDateAvailable) %>% select(date, VIX)
  real <- df_planetMood_1 %>% filter(between(date,firstDateToForecast,lastDateToForecast)) %>% select(date,VIX_real=VIX)
  accuracy_pm <- 
    cbind(date_txn=closingRef$date, VIX_txn=closingRef$VIX, real, VIX_forecasted = as.numeric(VIX_forecasted)) %>% 
    mutate(
      action = as.character(ifelse(VIX_forecasted > VIX_txn, "BUY", "SELL")), 
      realChangePercent = 100*(VIX_real - VIX_txn)/VIX_txn, 
      predChangePercent = as.numeric(100*(VIX_forecasted - VIX_txn)/VIX_txn), 
      success = as.logical((realChangePercent * predChangePercent) > 0),
      txnLength = date - date_txn, 
      earningsPercent = as.numeric(ifelse(success, abs(realChangePercent), -1*abs(realChangePercent)))
    )
  return(accuracy_pm)
}


#========================================================
# Prepare data
#========================================================

# PlanetMood dataset
df_planetMood <- readRDS("data/df_planetMood.rds") %>% arrange(date) %>% 
  mutate(VIX = VIX+30, Year = year(date)) # Dataset ready for analysis 
allVbles <- names(df_planetMood)
# allVbles <- c("VIX", "Gold", "SP500", "VVIX", "VIX3M", "VIXNsdq", "GoldVlty", "VIX_HLvol", "VVIX_HLvol", "VIX3M_HLvol", "VIXNsdq_HLvol", "GoldVlty_HLvol", "Gold_HLvol", "SP500_HLvol", "NewsTone", "Goldstein", "IAI", "DAI1", "DAI2", "DAI3", "BCI", "CCI", "CLI", "Flights", "Tempo", "Energy", "Danceability", "MoonPhase", "WkDay", "YrWeek", "Year")
selectedVbles <- c("VIX", "VVIX", "VIX3M", "VIXNsdq", "GoldVlty", "DAI3", "CCI")
laggedVbles <- paste0(selectedVbles,"_n")
calendarVbles <- c("MoonPhase", "WkDay", "YrWeek", "Year")
df_planetMood_1 <- df_planetMood %>% 
  select(date, 
         selectedVbles, 
         calendarVbles)

#========================================================
#========================================================
# Modelling
#========================================================
#========================================================


# ------------------------------
# NEURAL NETWORKS WITH REGRESSORS
# https://bookdown.org/singh_pratap_tejendra/intro_time_series_r/neural-networks-in-time-series-analysis.html


accuracies_all <- data.frame()
examplesToGenerate <- 10
daysToForecast <- 21
lagToApply <- daysToForecast
sampleDates <- sort(sample(as_date(c(as_date("2021-01-01"):(max(df_planetMood$date)-lagToApply))), examplesToGenerate, replace=TRUE))

for (i in sampleDates) {
  # ----
  lastDateAvailable <- as_date("2022-06-01") # last date available for training observations
  print(paste0("Processing date: ", as_date(i)))
  lastDateAvailable <- as_date(i) # last date available for training observations
  firstDateToForecast <- lastDateAvailable + 1
  lastDateToForecast <- firstDateToForecast + daysToForecast - 1
  # add a lagged columns to dataset:
  df_planetMood_1 <- df_planetMood %>% mutate(
    VIX_n = lag(VIX, n=lagToApply), 
    VVIX_n = lag(VVIX, n=lagToApply), 
    VIX3M_n = lag(VIX3M, n=lagToApply), 
    VIXNsdq_n = lag(VIXNsdq, n=lagToApply), 
    GoldVlty_n = lag(GoldVlty, n=lagToApply), 
    DAI3_n = lag(DAI3, n=lagToApply), 
    CCI_n = lag(CCI, n=lagToApply)
  ) %>% 
    filter(!is.na(VIX_n)) # we remove lines with NAs generated by lagging
  # prepare training dataset (past)
  df_planetMood_train <- df_planetMood_1 %>% 
    select(date, VIX, laggedVbles, calendarVbles) %>% 
    filter(date >= "2017-01-01" & date <= lastDateAvailable)
  # calculate regressors (future)
  futureData <- df_planetMood_1 %>% 
    select(date, laggedVbles, calendarVbles) %>% 
    filter(date >= firstDateToForecast & date <= lastDateToForecast)
  # ----
  # train
  yTrain <- df_planetMood_train$VIX
  xTrain <- df_planetMood_train[,3:13]  # remove date and VIX
  fit6 <- nnetar(ts(yTrain, frequency = 7), xreg = xTrain)
  # forecast
  xFuture <- futureData[,2:12] # remove date
  fc6 <- forecast(fit6, h = daysToForecast, xreg = xFuture, PI = F)
  # store
  VIX_forecasted <- fc6$mean
  accuracy_pm_NNETAR_xreg <- calculateAccuracyDataframe_pm(VIX_forecasted)
  matplot(accuracy_pm_NNETAR_xreg[,c(2, 4:5)], type = "b", pch=1, col = c(1, 3,2))
  legend("bottomleft", legend = c("VIX_txn", "VIX_real", "VIX_forecasted"), col= c(1, 3,2), pch=1)
  accuracies_all <- rbind(accuracies_all, accuracy_pm_NNETAR_xreg)
  class(accuracy_pm_NNETAR_xreg)
  # show
  accuracy_pm_NNETAR_xreg
}
print("Process finished")
accuracies_all
sum(accuracies_all$earningsPercent)
mean(accuracies_all$success)
saveRDS(accuracies_all,"data/test_accuracies.rds")



