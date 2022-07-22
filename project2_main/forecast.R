# JES!: MODELO1... EDA para decidir sólo con regresores de YahooFinance e ir incorporándolos cuando el proceso esté montado
  # decidir daysToForecast = ¿2/!3/4/!5/7/9/10/14?
  # probar transformations: log(), scale(), diff()
  # añadir month(), dayInMonth()
# JES!: EXTRACT...
  # probar length desde "2015-01-01" ¿velocidad&accuracy?
# JES!: MODELO2...
  # crear modelo lm/tree básico añadiendo weekday(), month(), dayInMonth(), weekInYear()
  # añadir algún indicador de "sensibilidad" (VVIX, ¿IAI?)
# JES!: PLATAFORMA...
  # montar proceso integral (ETL + forecast + prediction + publish)
  # probar AWS para programar diariamente y enviar mail
  # crear shinnyApp
# JES: MODELO1... 
  # jugar con más parámetros de nnetar y de forecast
# JES: MODELO2... 
  # refinar más vblesPlanetMood (movingAverage/diff/log/smooth)




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
daysToForecast <- 9  # horizon for forecast
transformation <- ""
# regressors_set <- "VX+C1" # ("VIX_n", "VVIX_n", "VIX3M_n", "VIXNsdq_n", "GoldVlty_n")

# ------------------------------------------------------
# CONSTANTS
examplesToGenerate <- 300  # 0 means TODAY
frequencyNN <- 365  # daily frequency for our data (time series use year as base unit)
lagToApply <- daysToForecast

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
daysToForecast <- 1  # horizon for forecast
recommendationsNN <- generateRecommendations(dataUptodate, examplesToGenerate, lagToApply)
daysToForecast <- 3  # horizon for forecast
recommendationsNN <- generateRecommendations(dataUptodate, examplesToGenerate, lagToApply)
daysToForecast <- 5  # horizon for forecast
recommendationsNN <- generateRecommendations(dataUptodate, examplesToGenerate, lagToApply)
daysToForecast <- 8  # horizon for forecast
recommendationsNN <- generateRecommendations(dataUptodate, examplesToGenerate, lagToApply)
daysToForecast <- 11  # horizon for forecast
recommendationsNN <- generateRecommendations(dataUptodate, examplesToGenerate, lagToApply)
daysToForecast <- 12  # horizon for forecast
recommendationsNN <- generateRecommendations(dataUptodate, examplesToGenerate, lagToApply)
daysToForecast <- 13  # horizon for forecast
recommendationsNN <- generateRecommendations(dataUptodate, examplesToGenerate, lagToApply)



recommendationsNN
# analyze results
tmpRecs <- readRDS("project2_main/recommendationsNN_all.RDS") %>% filter(length>=1904)    # as_date("2017-01-01") + 1904 = "2022-03-20"
grpRecs <- tmpRecs %>% 
  group_by(transformations, action, horizon, txnLength = as.integer(txnLength)) %>% 
  summarise(n = n(), Mean_TxnEarning = mean(earningsPercent), Mean_success = mean(success)) %>% 
  filter()
grpRecs%>% arrange(desc(Mean_success))
grpRecs%>% arrange(desc(Mean_TxnEarning))
view(grpRecs)

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



