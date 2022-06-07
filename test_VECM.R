# PTE: PARAMETRIZAR LÍNEA 158 VARIABLES...! (Y/O QUITAR ^ DE VIX Y VVIX)
# PTE: horizon: 90 días?
# Añadir Gold
# PTE: vbles endógenas: quarters/estacionalidad_4_seasons?/weekDay?/yearWeek?/faseLunar?
# Entender y decidir entre VAR y VECM




# install.packages("vars")
# install.packages("tsDyn")

source("10_initialize.R")

#========================================================#
# Quantitative ALM, Financial Econometrics & Derivatives 
# https://www.r-bloggers.com/2021/12/vector-error-correction-model-vecm-using-r/
#
# https://kiandlee.blogspot.com
#——————————————————-#
# Vector Error Correction Model and Cointegration
#========================================================#

graphics.off()  # clear all graphs
rm(list = ls()) # remove all files from your workspace
library(tidyverse)
library(urca) # ca.jo, denmark
library(vars) # vec2var
select <- dplyr::select # vars uses package MASS, which "select" function causes conflicts with dplyr::select
library(tsDyn) # VECM

#========================================================
# Data
#========================================================

df_planetMood <- readRDS("data/df_planetMood.rds")  # Dataset ready for analysis 
num_test_records <- round(nrow(df_planetMood)*0.2)
df_planetMood_test <- df_planetMood[(nrow(df_planetMood)-num_test_records+1):nrow(df_planetMood),]
df_planetMood_train <- df_planetMood[1:(nrow(df_planetMood)-num_test_records),]

# selected variables
allVbles <- c("VIX", "VVIX", "Flights", "Tempo", "Energy", "Danceability", "BCI_DE", "CCI_DE", "CLI_DE", "IAI", "NewsTone", "Goldstein", "MoonPhase", "WkDay", "YrWeek", "DAI1", "DAI2", "DAI3")
selectedVbles_2 <- c("VIX", "VVIX")
selectedVbles_3 <- c("VIX", "VVIX", "Tempo")
selectedVbles_4 <- c("VIX", "VVIX", "Tempo", "NewsTone")
selectedVbles_1 <- c("NewsTone")
selectedVbles_15 <- c("VIX", "VVIX", "Flights", "Tempo", "Energy", "Danceability", "BCI_DE", "CCI_DE", "CLI_DE", "IAI", "NewsTone", "Goldstein", "DAI1", "DAI2", "DAI3")

# Draw Graph 2x1
par(mfrow=c(2,1), mar=c(5,3,3,3))
for(i in 1:2) {
  matplot(df_planetMood_train[,selectedVbles_2][,i], axes=FALSE,
          type=c('l'), col = c('blue'), 
          main = names(df_planetMood_train[,selectedVbles_2])[i])
  axis(2) # show y axis
  axis(1, at=seq_along(1:nrow(df_planetMood_train)),
       labels=df_planetMood_train$date, las=2)
}

# Draw Graph 2x2
par(mfrow=c(2,2), mar=c(5,3,3,3))
for(i in 1:4) {
  matplot(df_planetMood_train[,selectedVbles_4][,i], axes=FALSE,
          type=c('l'), col = c('blue'), 
          main = names(df_planetMood_train[,selectedVbles_4])[i])
  axis(2) # show y axis
  axis(1, at=seq_along(1:nrow(df_planetMood_train)),
       labels=df_planetMood_train$date, las=2)
}

# Draw Graph 3x5
par(mfrow=c(3,5), mar=c(5,3,3,3))
for(i in 1:15) {
  matplot(df_planetMood_train[,selectedVbles_15][,i], axes=FALSE,
          type=c('l'), col = c('blue'), 
          main = names(df_planetMood_train[,selectedVbles_15])[i])
  axis(2) # show y axis
  axis(1, at=seq_along(1:nrow(df_planetMood_train)),
       labels=df_planetMood_train$date, las=2)
}

# Draw Graph 1
df_planetMood_train %>% ggplot(aes(x = date, y = VIX)) + 
  geom_line() + 
  scale_x_date(date_labels = "%Y-%m")









# Draw Graph 1-1
# df_planetMood_train_long <- df_planetMood_train %>% gather(key = "Feature", value = "Valores", 2:ncol(df_planetMood_train))
# df_planetMood_train_long %>% filter(Feature %in% c("VIX", "VVIX")) %>% ggplot(aes(date, Valores, colour=Feature)) + geom_point()

vblesToCompare <- c("VIX", "VVIX")
df_planetMood_train[,c("date", vblesToCompare)] %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = VIX, colour = vblesToCompare[1])) +
  geom_line(aes(y = VVIX, color = vblesToCompare[2])) + 
  scale_y_continuous("VVIX", sec.axis = sec_axis(~./10, name = "Relative humidity [%]"))
  ggtitle("Variables comparison")

library(xts)    
testData <- df_planetMood_train[,c("date", selectedVbles_4)]
testData <- as.xts(testData [, -1], order.by = testData$date)
plot(testData, main = "VIX")

pairs(coredata(testData), lower.panel = NULL)

# install.packages("tseries")
library(tseries)
testData2 <- as_tibble(testData)
ccf(testData2$VIX, testData2$VVIX, lag=90, pl=TRUE)

# install.packages("astsa")
library(astsa)
VIX <- testData2$VIX
VVIX <- testData2$VVIX
lag2.plot(VVIX, VIX, 40)









str.main <- selectedVbles_4
lev <- df_planetMood_train[,str.main]
nr_lev <- nrow(lev)

# quarterly centered dummy variables
yq <- data.frame(q = as.numeric(quarter(df_planetMood_train$date)), year = year(df_planetMood_train$date))
rownames(yq) <- NULL
yq$Q1 <- (yq$q==1)-1/4
yq$Q2 <- (yq$q==2)-1/4
yq$Q3 <- (yq$q==3)-1/4
dum_season <- yq[,-c(1,2)]

# 1st differenced data
dif <- as.data.frame(diff(as.matrix(lev), lag = 1))

#========================================================
# Cointegration Test
#========================================================

#———————————————-
# Johansen Cointegration Procedure
#———————————————-
# ecdet  = ‘none’  for no intercept 
#          ‘const’ for constant term
#          ‘trend’ for trend variable 
#          in cointegration
# type   =  eigen or trace test
# K      =  lag order of VAR
# spec   = 'transitory' or 'longrun'
# season = centered seasonal dummy (4:quarterly)
# dumvar = another dummy variables
#———————————————-

# 4 variables:
coint_ca.jo <- ca.jo(lev, 
                     ecdet = 'none', type  = 'eigen', K = 2, 
  spec = 'transitory', season = 4, dumvar = NULL)
summary(coint_ca.jo)

# n variables:
# selectedVbles_n <- c(names(lev), c("Energy", "Danceability", "BCI_DE", "CCI_DE"))
# coint_ca.jo <- ca.jo(selectedVbles_n, 
#                      ecdet = 'none', type  = 'eigen', K = 2, 
#                      spec = 'transitory', season = 4, dumvar = NULL)
# summary(coint_ca.jo)

#========================================================
# VECM model estimation
#========================================================

#————————————————
# VECM estimation
#————————————————
# VECM(data, lag, r = 1, 
#      include = c('const', 'trend', 'none', 'both'),
#      beta = NULL, estim = c('2OLS', 'ML'), 
#      LRinclude = c('none', 'const','trend', 'both'), 
#      exogen = NULL)
#————————————————

VECM_tsDyn <- VECM(lev, lag=1, r=2,
                   estim = 'ML',
                   LRinclude = 'none',
                   exogen = dum_season)
summary(VECM_tsDyn)

#————————————————
# restricted VECM -> input for r
#————————————————
cajorls_ca.jo <- cajorls(coint_ca.jo, r=2)

#————————————————
# the VAR representation of a VECM from ca.jo
#————————————————
# vec2var: Transform a VECM to VAR in levels
# ca.jo is transformed to a VAR in level
# r : The cointegration rank 
#————————————————
vec2var_ca.jo <- vec2var(coint_ca.jo, r=2)


#========================================================
# Estimation Results
#========================================================

#———————————————-
# parameter estimates from each model
#———————————————-
VECM_tsDyn
cajorls_ca.jo
vec2var_ca.jo


#========================================================
# Forecast
#========================================================

# forecasting horizon
nhor <- 12 

#———————————————-
# Forecast from VECM() in tsDyn
#———————————————-

# quarterly centered dummy variables for forecast
dumf_season <- rbind(tail(dum_season,4),
                     tail(dum_season,4),
                     tail(dum_season,4))

VECM_pred_tsDyn <- predict(VECM_tsDyn, 
                           exoPred = dumf_season, n.ahead=nhor)

# Draw Graph
par(mfrow=c(4,1), mar=c(2,2,2,2))

# historical data + forecast data
df <- rbind(lev, VECM_pred_tsDyn)

for(i in 1:4) {
  matplot(df[,i], type=c('l'), col = c('blue'), 
          main = str.main[i]) 
  abline(v=nr_lev, col='blue')
}

VECM_pred_tsDyn

# JES: delta forecasted:
print("Last known values for VIX:")
print(df_planetMood_train[(nrow(df_planetMood_train)-6):(nrow(df_planetMood_train)),"VIX"])
print("Prediction for VIX:")
print(VECM_pred_tsDyn[,1])
print("Actual for VIX:")
print(df_planetMood_test[1:nhor,"VIX"])
(VECM_pred_tsDyn[,"VIX"] - -7.8)

#———————————————-
# Forecast from ca.jo() using vec2var()
#———————————————-

pred_vec2var_ca.jo <- predict(vec2var_ca.jo, n.ahead=nhor)

par(mai=rep(0.4, 4)); plot(pred_vec2var_ca.jo)
par(mai=rep(0.4, 4)); fanchart(pred_vec2var_ca.jo)

# !! PARAMETRIZAR ESTAS VARIABLES...! (Y/O QUITAR ^ DE VIX Y VVIX)
m.pred_vec2var_ca.jo <- cbind(
  pred_vec2var_ca.jo$fcst$stocks..VIX_GLOBAL[,1], 
  pred_vec2var_ca.jo$fcst$stocks..VVIX_GLOBAL[,1],
  pred_vec2var_ca.jo$fcst$music.tempo_GLOBAL[,1], 
  pred_vec2var_ca.jo$fcst$GDELT.tone_GLOBAL[,1])

colnames(m.pred_vec2var_ca.jo) <- colnames(lev)

m.pred_vec2var_ca.jo

#———————————————-
# Comparison of two sets of forecast
#———————————————-

VECM_pred_tsDyn - m.pred_vec2var_ca.jo

