# ! CRON: rematar + quitar email (password) 
# ! MODELO1... confirmar/descartar NN con regressors "n-1"
# ! MODELO1... con EDA ¿confirmar multiHorizon? ¡descartar algún horizonte?
# ! MODELO1... valorar: LM_n-2 vs. NN+tree vs. VAR
# ! MODELO1... probar de nuevo scale/log con NN (y otros posible métodos)
# MODELO1... jugar con más parámetros de nnetar() y de forecast() + probar transformations (scale(), log(), BoxCox-lambda)
# MODELO1 y/o MODELO2... refinar más vblesPlanetMood (movingAverage/diff/log/smooth)
# ! MODELO2... con EDA crear llamada básica de "filtrado" añadiendo length(!), weekday(), month(), dayInMonth(), weekInYear() y algún indicador de "sensibilidad instantánea" (VIX_+-30, VVIX, ¿IAI?)
# ! MONTAR PROCESO INTEGRAL... (ETL + forecast + prediction + .rds + shiny + email) + comprobar valor última fecha cargada (Imputation + Hora exacta cierre)
# PTE: depurar paso final (y lentitud + warnings) de la extracción de datos de Yahoo:
# `summarise()` regrouping output by 'date' (override with `.groups` argument)
# Note: Using an external vector in selections is ambiguous.
# ℹ Use `all_of(selectedVbles)` instead of `selectedVbles` to silence this message.
# ℹ See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
# This message is displayed once per session.
# PTE: quitar warnings de nnetar()
    # WARNING messages:
    # 1: In nnetar(ts(yTrain, frequency = frequencyNN), xreg = xTrain) :
    # Missing values in xreg, omitting rows
# PTE: en AWS, instalar KeyPair para SSH (terminal) de EC2/AMI, para que no pida password al hacer $sudo
# PTE: en AWS, instalar SSH RSA en el enlace de GIT con RStudioServer reemplazando a PERSONALAccessToken para que no lo pida siempre que se hace $git push


source("./project2_main/initialize.R")
source("./project2_main/extractDataUptodate.R")
dataUptodate <- readRDS("./project2_main/dataUptodate.rds") #  load last available fresh daily data (prescriptors)

# ------------------------------------------------------
# generate recommendations based in the forecast using NNETAR with regressors
# run the NN to generate recommendations:

source("./project2_main/generateRecommendations.R")  # FUNCTION to generate recommendations using NN forecast
examplesToGenerate <- 1  # 0 means TODAY
for (j in c(7)) {
  print (paste("=====> HORIZON:",j))
  daysToForecast <- j  # horizon for forecast
  lagToApply <- daysToForecast
  recommendationsNN <- generateRecommendations(dataUptodate, examplesToGenerate, lagToApply)
}
readRDS("./project2_main/recommendationsNN_all.rds")
head(recommendationsNN)
tail(recommendationsNN)


# --------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------
# MODEL 2
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



