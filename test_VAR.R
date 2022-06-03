# RMRK: quarters, estacioalidad



# install.packages("vars")
# install.packages("tsDyn")
source("10_initialize.R")


#========================================================#
# VAR ()Vector Autoregressive Model
# https://www.r-bloggers.com/2021/11/vector-autoregressive-model-var-using-r/
#
# https://kiandlee.blogspot.com
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

# forecasting horizon
nhor <- 12

df_planetMood <- readRDS("data/df_planetMood.rds")  # Dataset ready for analysis 
names(df_planetMood)
head(as_tibble(df_planetMood))

# selected variables
df.lev <- df_planetMood[,c('stocks.^VIX_GLOBAL','stocks.^VVIX_GLOBAL','music.tempo_GLOBAL','GDELT.tone_GLOBAL')]
m.lev  <- as.matrix(df.lev)
nr_lev <- nrow(df.lev)

# quarterly centered dummy variables
dum_season <- data.frame(yyyymm = df_planetMood$date)
substr.q   <- as.numeric(quarter(df_planetMood$date))
dum_season$Q2 <- (substr.q==2)-1/4
dum_season$Q3 <- (substr.q==3)-1/4
dum_season$Q4 <- (substr.q==4)-1/4
dum_season    <- dum_season[,-1]

# Draw Graph
str.main <- c('^VIX','^VVIX','music.tempo_GLOBAL','GDELT.tone')
x11(width=12, height = 6); 
par(mfrow=c(2,2), mar=c(5,3,3,3))
for(i in 1:4) {
  matplot(m.lev[,i], axes=FALSE,
          type=c('l'), col = c('blue'), 
          main = str.main[i])
  axis(2) # show y axis
  # show x axis and replace it with 
  # an user defined sting vector
  axis(1, at=seq_along(1:nrow(df.lev)),
       labels=df_planetMood$date, las=2)
}

#========================================================
# VAR model in level
#========================================================

# lag length p
# Lag length (p) is selected by using several information criteria : AIC, HQ, SC, and so on. Lower these scores are better since these criteria penalize models that use more parameters. This work can be done easily by using VARselect() function with a maximum lag. From the following results, we set lag lengths of VAR in level and VAR in difference to 2 and 1 respectively.
VARselect(df.lev, lag.max = 4, type = 'const', season = 4)

# estimation
var.model_lev <- VAR(df.lev, p = 2, type = 'const', season = 4)

# forecast of lev data
var.pred <- predict(var.model_lev, n.ahead = nhor)
x11(); par(mai=rep(0.4, 4)); plot(var.pred)
x11(); par(mai=rep(0.4, 4)); fanchart(var.pred)


#========================================================
# VAR model in difference using vars
#========================================================

# 1st differenced data
df.diff <- diff(as.matrix(df.lev), lag = 1)
colnames(df.diff) <- c('dVIX','dVVIX','dtempo','dtone')
m.diff <- as.matrix(df.diff)

# lag length
VARselect(df.diff, lag.max = 4, type = 'const', season = 4)

# estimation
vare_diff <- VAR(df.diff, p = 1, type = 'const', season = 4)

# forecast of differenced data
varf_diff <- predict(vare_diff, n.ahead = nhor)
x11(); par(mai=rep(0.4,4)); plot(varf_diff)
x11(); par(mai=rep(0.4,4)); fanchart(varf_diff)

# recover lev forecast
m.varf_lev_ft <- rbind(m.lev, matrix(NA, nhor, 4))
m.ft_df <- do.call(cbind,lapply(varf_diff$fcst, 
                                function(x) x[,'fcst']))

# growth to level
for(h in (nr_lev+1):(nr_lev+nhor)) {
  hf <- h - nr_lev
  m.varf_lev_ft[h,] <- m.varf_lev_ft[h-1,] + m.ft_df[hf,]
}

# Draw Graph
x11(width=8, height = 8); 
par(mfrow=c(4,1), mar=c(2,2,2,2))

for(i in 1:4) {
  df <- m.varf_lev_ft[,i]
  matplot(df, type=c('l'), col = c('blue'), 
          main = str.main[i]) 
  abline(v=nr_lev, col='blue')
}


#========================================================
# VAR model in difference using tsDyn
#========================================================

linevare_diff <- lineVar(data = df.lev, lag = 1, include = 'const',
                         model = 'VAR', I = 'diff', beta = NULL, exogen = dum_season)

# check if both models (vars & tsDyn) yield same coefficients
linevare_diff 
do.call(rbind,lapply(vare_diff$varresult, 
                     function(x) x$coefficients))

# quarterly centered dummy variables for forecast
dumf_season <- rbind(tail(dum_season,4),
                     tail(dum_season,4),
                     tail(dum_season,4))
# forecast
linevarf_diff <- predict(linevare_diff, n.ahead = nhor, 
                         exoPred = dumf_season) 
# Draw Graph
x11(width=8, height = 8); 
par(mfrow=c(4,1), mar=c(2,2,2,2))

df <- rbind(df.lev, linevarf_diff)

for(i in 1:4) {
  matplot(df[,i], type=c('l'), col = c('blue'), 
          main = str.main[i]) 
  abline(v=nr_lev, col='blue')
}

