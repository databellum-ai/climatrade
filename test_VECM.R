# PTE: horizon: 90 días?
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


#========================================================
# VECM
#========================================================

str.main <- c("VIX", "VVIX", "Tempo", "NewsTone", "Gold", "IAI")
outcomesList <- c("VIX", "Gold")
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

coint_ca.jo <- ca.jo(lev, ecdet = 'none', type  = 'eigen', K = 2, spec = 'transitory', season = 7, dumvar = NULL)
summary(coint_ca.jo)

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

VECM_tsDyn <- VECM(lev, lag=1, r=2, estim = 'ML', LRinclude = 'none', exogen = dum_season)
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

VECM_pred_tsDyn <- predict(VECM_tsDyn, exoPred = dumf_season, n.ahead=nhor)


# historical data + forecast data
df <- rbind(lev, VECM_pred_tsDyn)
# Draw Graph
par(mfrow=c(6,1), mar=c(2,2,2,2))
for(i in 1:6) {
  matplot(df[,i], type=c('l'), col = c('blue'), 
          main = str.main[i]) 
  abline(v=nr_lev, col='blue')
}

numRows <- nrow(lev)
df_planetMood[(numRows-3):(numRows+3),c("date", str.main)]   # All data (training + test)
lev[(numRows-12):numRows,]  # Training history
VECM_pred_tsDyn # Forecast
df_planetMood_test[1:12,str.main] # Actual data

# JES: delta forecasted:
print("Last known values for VIX:")
print(df_planetMood_train[(nrow(df_planetMood_train)-6):(nrow(df_planetMood_train)),"VIX"])
print("Prediction for VIX:")
print(VECM_pred_tsDyn[,1])
print("Actual for VIX:")
print(df_planetMood_test[1:nhor,"VIX"])

#———————————————-
# Forecast from ca.jo() using vec2var()
#———————————————-

pred_vec2var_ca.jo <- predict(vec2var_ca.jo, n.ahead=nhor)

par(mai=rep(0.4, 4)); plot(pred_vec2var_ca.jo)
par(mai=rep(0.4, 4)); fanchart(pred_vec2var_ca.jo)

# Outcomes: VIX, Gold
m.pred_vec2var_ca.jo <- cbind(
  pred_vec2var_ca.jo$fcst$VIX[,1], 
  pred_vec2var_ca.jo$fcst$Gold[,1])

colnames(m.pred_vec2var_ca.jo) <- outcomesList


#———————————————-
# Comparison of two sets of forecast
#———————————————-

VECM_pred_tsDyn[,outcomesList] - m.pred_vec2var_ca.jo

