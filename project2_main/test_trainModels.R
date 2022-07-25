# !!! confirmar/descartar con regressors "n-1"
# !! MODELO2... con EDA crear llamada básica de "filtrado" añadiendo length(!), weekday(), month(), dayInMonth(), weekInYear() y algún indicador de "sensibilidad instantánea" (VIX_+-30, VVIX, ¿IAI?)
# ! montar proceso integral (ETL + forecast + prediction + publish) + comprobar valor última fecha cargada (Imputation + Hora exacta cierre)
# ! quitar warnings de Extract (YahooFinance) (..."silence()")
# crear modelo lm/tree a partir de la llamada básica de filtrado de recomendaciones
# probar AWS para programar diariamente y enviar mail
# crear shinnyApp
# conn EDA ¿confirmar multiHorizon? ¡descartar algún horizonte?
# jugar con más parámetros de nnetar y de forecast + probar transformations (scale(), log(), BoxCox-lambda)
# refinar más vblesPlanetMood (movingAverage/diff/log/smooth) para meter en MODELO1 y/o MODELO2
# quitar warnings de nnetar()
    # WARNING messages:
    # 1: In nnetar(ts(yTrain, frequency = frequencyNN), xreg = xTrain) :
    # Missing values in xreg, omitting rows


source("project2_main/initialize.R")

transformation <- ""

# ------------------------------------------------------
# extract daily data from live sources from history until last close
dataUptodate <- extractDataUptodate()
head(dataUptodate)
# all recommendations generated are consolidated in a RDS for further analysis
saveRDS(dataUptodate,"project2_main/dataUptodate.rds") #  save last available fresh daily data
dataUptodate <- readRDS("project2_main/dataUptodate.rds") #  load last available fresh daily data (prescriptors)

# ------------------------------------------------------
# generate recommendations based in the forecast using NNETAR with regressors
# run the NN to generate recommendations:
examplesToGenerate <- 50  # 0 means TODAY

for (j in c(7,14)) {
  print (paste("=====> HORIZON:",j))
  daysToForecast <- j  # horizon for forecast
  lagToApply <- daysToForecast
  recommendationsNN <- generateRecommendations(dataUptodate, examplesToGenerate, lagToApply)
}

recommendationsNN
# analyze results
tmpRecs <- readRDS("project2_main/recommendationsNN_all.RDS") # %>% filter(length>=1904)    # as_date("2015-01-01") + 2616 = "2022-03-20"

grpRecs <- tmpRecs %>%
  group_by(
    regressors,
    # action, 
    # volatility = (VIX_txn>30),
    horizon, txnLength = as.integer(txnLength)) %>%
  summarise(n = n(), Mean_TxnEarning = mean(earningsPercent), Mean_success = mean(success)) %>%
  filter()
grpRecs%>% arrange(desc(Mean_success))
grpRecs%>% arrange(desc(Mean_TxnEarning))

tmpRecs %>% 
  filter(horizon == 7) %>% 
  group_by(regressors, horizon, txnLength) %>%
  summarise(n = n(), Mean_TxnEarning = mean(earningsPercent), Mean_success = mean(success)) %>% 
  arrange(desc(Mean_success)) %>% 
  arrange(horizon, desc(txnLength)) %>% 
  head(20)





# --------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------

# train a regression model (or tree etc.) to optimize selection of recommendations to implement
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



