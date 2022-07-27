
# =====================================
# REAL PROCESS = FORECAST + PREDICTION
# =====================================


source("./project2_main/initialize.R")
source("./project2_main/extractDataUptodate.R")
dataUptodate <- readRDS("./project2_main/dataUptodate.rds") #  load last available fresh daily data (prescriptors)

# ------------------------------------------------------
# generate recommendations based in the forecast using NNETAR with regressors
# run the NN to generate recommendations:

source("./project2_main/generateRecommendations.R")  # FUNCTION to generate recommendations using NN forecast
examplesToGenerate <- 1  # 0 means TODAY
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
  readRDS("./project2_main/recommendationsNN_all.rds") %>%
  select("VIX_txn", "VIX_forecasted", "predChangePercent", "txnLength", "success")

testREV <- cbind(
  freshRecommendations, 
  revisedSuccess = predict.glm(glmSuccess, newdata = freshRecommendations)) %>% 
  arrange(txnLength )
testREV

# saveRDS(dataset_21d,"data/recomendationsNN_21d.rds")



