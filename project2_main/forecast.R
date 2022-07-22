# JES!: MODELO1... EDA para decidir sólo con regresores de YahooFinance e ir incorporándolos cuando el proceso esté montado
  # ¿daysToForecast = 3/7/10/14?
  # ¿"DAI3_n", "CCI_n"?
  # ¿frequencyNN=365 vs 7?
  # ¿weekday(), month()?
  # dayInMonth(), weekInYear()}
  # ¿transformations log(), scale(), diff()?
# JES!: MODELO2...
  # crear modelo lm/tree básico añadiendo weekday(), month(), dayInMonth(), weekInYear()
# JES!: PLATAFORMA...
  # montar proceso integral (ETL + forecast + prediction + publish)
  # probar AWS para programar diariamente y enviar mail
  # crear shinnyApp
# JES: MODELO2... 
  # refinar más vblesPlanetMood (movingAverage/diff/log/smooth)
# JES: MODELO1... 
  # probar VAR (Haydn + Tajendra) para forecast de regressors (*_n) (actuales y vblesPlanetMood)
  # jugar con más parámetros de nnetar y de forecast
  # probar NN con ejemplo de keras (carpeta tests)



rm(list = ls())  # clear all environment variables
graphics.off()  # clear all graphs
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(forecast)) install.packages("forecast", repos = "http://cran.us.r-project.org")
if(!require(fpp3)) install.packages("fpp3", repos = "http://cran.us.r-project.org")
# if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(forecast)
library(fpp3)
# library(GGally)

# ------------------------------------------------------
# HYPERPARAMETERS
daysToForecast <- 14  # horizon for forecast
lagToApply <- daysToForecast
frequencyNN <- 365  # seasonality a priori for NNETAR model
examplesToGenerate <- 300  # 0 means: TODAY


# ------------------------------------------------------
# INCLUDED FUNCTIONS
source("project2_main/extractDataUptodate.R")  # function to generate recommendations using NN forecast
source("project2_main/generateRecommendations.R")  # function to generate recommendations using NN forecast

# ------------------------------------------------------
# extract daily data from live sources from history until last close
dataUptodate <- extractDataUptodate()
head(dataUptodate)
saveRDS(dataUptodate,"project2_main/dataUptodate.rds") #  save last available fresh daily data


# ------------------------------------------------------
# generate recommendations based in the forecast using NNETAR with regressors
# all recommendations generated are consolidated in a RDS for further analysis
dataUptodate <- readRDS("project2_main/dataUptodate.rds") #  load last available fresh daily data (prescriptors)

# run the NN to generate recommendations based in a forecast:
recommendationsNN <- generateRecommendations(
    dataUptodate, 
    examplesToGenerate, 
    lagToApply)

recommendationsNN[,-13]
tmpRecs <- recommendationsNN
tmpRecs <- readRDS("project2_main/recommendationsNN_all.RDS")
tmpRecs %>% group_by(freq, horizon, xreg = str_sub(regressors,-30,-1), transformations, action, txnLength) %>% 
  summarise(Mean_success = mean(success), Sum_Earnings = sum(earningsPercent), Avg_TxnEarning = mean(earningsPercent)/mean(horizon), n = n()) %>% 
  arrange(desc(Mean_success)) %>% 
  filter(n >=5)

# print(paste0("Total balance recommendations generated: ", round(sum(recommendationsNN$earningsPercent),1),"%"))
# print(paste0("Success of recommendations generated: ", round(100*mean(recommendationsNN$success),2),"%"))

# ------------------------------------------------------
# train a regression model to optimize selection of recommendations to implement
# https://www.r-bloggers.com/2015/09/how-to-perform-a-logistic-regression-in-r/
# training data
dataset_glm <- 
  readRDS("project2_main/recommendationsNN_all.rds") %>% 
  filter(horizon == daysToForecast) %>% 
  select("VIX_txn", "VIX_forecasted", "predChangePercent", "txnLength", "success")
head(dataset_glm)
# split dataset in training and test
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


# ------------------------------------------------------
# predict on real data
# prepare newdata (recommendations just obtained)
freshRecommendations <-
  readRDS("project2_main/recommendationsNN_all.rds") %>%
  select("VIX_txn", "VIX_forecasted", "predChangePercent", "txnLength", "success")

testREV <- cbind(
  freshRecommendations, 
  revisedSuccess = predict.glm(glmSuccess, newdata = freshRecommendations)) %>% 
  arrange(txnLength )
testREV

# saveRDS(dataset_21d,"data/recomendationsNN_21d.rds")



