
# =====================================
# REAL PROCESS = FORECAST + PREDICTION
# =====================================

source("project2_main/initialize.R")
transformation <- "NuevoExtractMultihorizonte + lambdaAuto+scale"

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
# recommendationsNN <- generateRecommendations(dataUptodate, examplesToGenerate, lagToApply)
# 
examplesToGenerate <- 0  # 0 means TODAY
for (j in c(2:14)) {
  print (paste("=====> HORIZON:",j))
  daysToForecast <- j  # horizon for forecast
  lagToApply <- daysToForecast
  recommendationsNN <- generateRecommendations(dataUptodate, examplesToGenerate, lagToApply)
}

recommendationsNN

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



