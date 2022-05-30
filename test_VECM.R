# VAR (Vector AutoRegression with Error Correction example)
# https://www.r-bloggers.com/2021/12/vector-error-correction-model-vecm-using-r/

#========================================================#
# Quantitative ALM, Financial Econometrics & Derivatives 
# ML/DL using R, Python, Tensorflow by Sang-Heon Lee 
#
# https://kiandlee.blogspot.com
#——————————————————-#
# Vector Error Correction Model and Cointegration
#========================================================#
# install.packages("vars")
# install.packages("tsDyn")

graphics.off()  # clear all graphs
rm(list = ls()) # remove all files from your workspace

library(urca)  # ca.jo, ur.df, finland
library(vars)  # vec2var
library(tsDyn) # VECM

#========================================================
# Data
#========================================================

# level data : 1958q1 - 1984q2
data(finland)
lev <- finland; nr_lev <- nrow(lev)

# the sample period
yq <- expand.grid(1:4, 1958:1984)[1:nr_lev,]
colnames(yq) <- c('q', 'yyyy'); rownames(yq) <- NULL

# quarterly centered dummy variables
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
coint_ca.jo <- ca.jo(
  lev, ecdet = 'none', type  = 'eigen', K = 2, 
  spec = 'transitory', season = 4, dumvar = NULL)
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

VECM_tsDyn <- VECM(lev, lag=1, r=2,
                   estim = 'ML',
                   LRinclude = 'none',
                   exogen = dum_season)

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
x11(width=8, height = 8); 
par(mfrow=c(4,1), mar=c(2,2,2,2))

# historical data + forecast data
df <- rbind(lev, VECM_pred_tsDyn)

for(i in 1:4) {
  matplot(df[,i], type=c('l'), col = c('blue'), 
          main = str.main[i]) 
  abline(v=nr_lev, col='blue')
}

VECM_pred_tsDyn

#———————————————-
# Forecast from ca.jo() using vec2var()
#———————————————-

pred_vec2var_ca.jo <- predict(vec2var_ca.jo, n.ahead=nhor)

x11(); par(mai=rep(0.4, 4)); plot(pred_vec2var_ca.jo)
x11(); par(mai=rep(0.4, 4)); fanchart(pred_vec2var_ca.jo)

m.pred_vec2var_ca.jo <- cbind(
  pred_vec2var_ca.jo$fcst$lrm1[,1], 
  pred_vec2var_ca.jo$fcst$lny[,1],
  pred_vec2var_ca.jo$fcst$lnmr[,1], 
  pred_vec2var_ca.jo$fcst$difp[,1])

colnames(m.pred_vec2var_ca.jo) <- colnames(lev)

m.pred_vec2var_ca.jo

#———————————————-
# Comparison of two sets of forecast
#———————————————-

VECM_pred_tsDyn - m.pred_vec2var_ca.jo

