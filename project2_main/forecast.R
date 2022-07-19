# JES: !usar regresión para optimizar criterio BUY/SELL en "accuracies_all" (previamente hacer cálculo masivo >= 100 fechas)
# JES: !en NN+xReg: usar log() y scale() para refinar
# JES: refinar más vblesPlanetMood (movingAverage/diff/log/smooth)
# JES: en NN+xReg:probar VAR (Haydn + Tajendra) para forecast de regressors (*_n) (actuales y vblesPlanetMood)
# JES: en NN+xReg: jugar con más parámetros de nnetar y de forecast
# JES: review/challente hyperparameters
# JES: crear shinnyApp


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# INITIALIZATION
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

# ===============
# ENVIRONMENT
# ---------------
rm(list = ls())  # clear all environment variables
graphics.off()  # clear all graphs
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(fpp3)) install.packages("fpp3", repos = "http://cran.us.r-project.org")
if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")
if(!require(forecast)) install.packages("forecast", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(fpp3)
library(GGally)
library(forecast)

#========================================================
# INCLUDED FUNCTIONS
# ---------------
source("generateRecommendations.R")  # function to generate recommendations using NN forecast


#========================================================
# HYPERPARAMETERS
# ---------------
daysToForecast <- 21  # horizon for forecast
frequencyNN <- 7  # seasonality a priori for NNETAR model


#========================================================
# PREPARE DATA FOR TRAINING AND VALIDATION
#========================================================
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
df_planetMood_1  # start building a dataset for forecasting

#========================================================
#========================================================
# GENERATOR OF BUY/SELL RECOMMENDATIONS USING FORECAST MODEL (NNETAR):
#========================================================
#========================================================
examplesToGenerate <- 1
lagToApply <- daysToForecast
datesRecommendations <- sort(sample(as_date(c(as_date("2021-01-01"):(max(df_planetMood$date)-lagToApply))), examplesToGenerate, replace=TRUE))
# recommendationsConsolidated <- 
#   generateRecommendations(as_date("2022-06-01"), examplesToGenerate, lagToApply, datesRecommendations)
recommendationsNN <- 
  generateRecommendations(datesRecommendations, examplesToGenerate, lagToApply, datesRecommendations)
recommendationsNN
sum(recommendationsNN$earningsPercent)
mean(recommendationsNN$success)


#========================================================
#========================================================
# LINEAR MODEL TO OPTIMIZE RECOMMENDATIONS
#========================================================
#========================================================
# training data
dataset_glm <- 
  readRDS("data/recommendationsNN_all.rds") %>% 
  filter(horizon == daysToForecast) %>% 
  select("VIX_txn", "VIX_forecasted", "predChangePercent", "txnLength", "success")
head(dataset_glm)
# entender/dibujar la mejora
# flujo de preparación de los datos (newdata, freshRecommendations, etc.)
# Probar LogisticRegression con "success"
# Mejorar parámetros de lm
# training VS test
# Meter weekdays de "date_txn", "date"
# Probar RegressionTrees, etc.
# guardar el modelo
# https://www.r-bloggers.com/2015/09/how-to-perform-a-logistic-regression-in-r/
splitRec <- round(0.8 * nrow(dataset_glm),0)
train_glm <- dataset_glm[1:splitRec,]
test_glm <- dataset_glm[(splitRec+1):nrow(dataset_glm),]
# train model
glmSuccess = 
  glm(
    success ~ VIX_txn + VIX_forecasted + predChangePercent + txnLength, 
    family=binomial(link='logit'), 
    data = train_glm)  # create the linear regression
summary(glmSuccess)#Review the results
plot(cooks.distance(glmSuccess), pch = 16, col = "blue")
# predict on test data splitted
predict.glm(glmSuccess, newdata = test_glm)


# predict on real data
# prepare newdata (recommendations just obtained)
freshRecommendations <-
  readRDS("data/freshRecommendationsNN.rds") %>%
  select("VIX_txn", "VIX_forecasted", "predChangePercent", "txnLength", "success")

testREV <- cbind(
  freshRecommendations, 
  revisedSuccess = predict.glm(lmSuccess, newdata = freshRecommendations)) %>% 
  arrange(txnLength )
testREV

# saveRDS(dataset_21d,"data/recomendationsNN_21d.rds")












