#========================================================#
# Forecasting: Principles and Practice (3rd ed)
# Rob J Hyndman and George Athanasopoulos
# 
# https://otexts.com/fpp3/
#========================================================#
  
source("10_initialize.R")

# install.packages("fpp3")
# install.packages("GGally")
library(fpp3)

#========================================================
# Data
#========================================================

df_planetMood <- readRDS("data/df_planetMood.rds")  # Dataset ready for analysis 

# PlanetMood dataset
df_planetMood <- readRDS("data/df_planetMood.rds")  # Dataset ready for analysis 
names(df_planetMood)
# convert to tsibble (time serie)
df_planetMood_ts <- df_planetMood %>% as_tsibble(index = date)  
# create a tidy (long) version
df_planetMood_tidy_ts <- df_planetMood %>% gather(key = "variable", value="value", c(-date)) %>% as_tsibble(index = date, key = variable)
# create a lagged dataset
lagsToGenerate <- 3
df_planetMood_ts_lagged <- NULL
lagged_all <- NULL
for (i in c(0:lagsToGenerate)) {
  lagged_all <- rbind(lagged_all, df_planetMood_ts[,-1] %>% lag(n = i) %>% mutate(lag = i))
  # %>% mutate(lag = i) 
  df_planetMood_ts_lagged <- cbind(df_planetMood_ts$date, lagged_all) %>% rename("date" = 1) %>% arrange(date)
}
df_planetMood_ts_lagged



#========================================================
# EDA - Charts
#========================================================

# Draw Graph 2x2
selectedVbles <- c("VIX", "VVIX", "Tempo", "NewsTone")
par(mfrow=c(2,2), mar=c(5,3,3,3))
for(i in 1:4) {
  matplot(df_planetMood[,selectedVbles][,i], axes=FALSE,
          type=c('l'), col = c('blue'), 
          main = names(df_planetMood[,selectedVbles])[i])
  axis(2) # show y axis
  axis(1, at=seq_along(1:nrow(df_planetMood)),
       labels=df_planetMood$date, las=2)
}


# Draw Graph 4x4
selectedVbles <- c("VIX", "Gold", "VVIX", "Flights", "Tempo", "Energy", "Danceability", "BCI", "CCI", "CLI", "IAI", "NewsTone", "Goldstein", "DAI1", "DAI2", "DAI3")
par(mfrow=c(4,4), mar=c(5,3,3,3))
for(i in 1:16) {
  matplot(df_planetMood[,selectedVbles][,i], axes=FALSE,
          type=c('l'), col = c('blue'), 
          main = names(df_planetMood[,selectedVbles])[i])
  axis(2) # show y axis
  axis(1, at=seq_along(1:nrow(df_planetMood)),
       labels=df_planetMood$date, las=2)
}


# Function to draw 1:1 comparison in two different axis
chartTwoAxis <- function(x, y1, y2, y1_name, y2_name) {
  par(mfrow=c(1,1))
  par(mar=c(5,5,5,5)+0.1, las=1)
  plot.new()
  plot.window(xlim=range(x), ylim=range(y1))
  lines(x, y1, col="red", pch=19, lwd=1)
  axis(1, col.axis="blue")
  axis(2, col.axis="red")
  box()
  plot.window(xlim=range(x), ylim=range(y2))
  lines(x, y2, col="limegreen", pch=19, lwd=1)
  axis(4, col.axis="limegreen")
  title(paste("1:1 compared history", y1_name, "vs", y2_name), adj=0)
  mtext(y2_name, side = 4, las=3, line=3, col="limegreen")
  mtext(y1_name, side = 2, las=3, line=3, col="red") }
# Call the function to draw 1:1 comparison in two different axis
chartTwoAxis(df_planetMood$date, df_planetMood$VIX, df_planetMood$VVIX, "VIX", "VVIX")


# Pairs correlations in date groups
library(xts)    
selectedVbles <- c("VIX", "VVIX", "Tempo", "NewsTone")
tmpData <- df_planetMood[,c("date", selectedVbles)]
tmpData <- as.xts(tmpData[, -1], order.by = tmpData$date)
group <- NA
firstQuartileDates <- as.Date(as.integer(summary(df_planetMood$date)[2]))
thirdQuartileDates <- as.Date(as.integer(summary(df_planetMood$date)[5]))
group[df_planetMood$date < firstQuartileDates] <- 1
group[df_planetMood$date >= firstQuartileDates & df_planetMood$date <= thirdQuartileDates] <- 2
group[df_planetMood$date > thirdQuartileDates] <- 3
pairs(coredata(tmpData), 
      lower.panel = NULL,
      col = c("red", "blue", "green")[group],
      pch = 18,
      main = "Pairs correlationscolorred by dates")


# Pairs correlations in date groups
# install.packages("tseries")
library(tseries)
yt <- df_planetMood$VIX
xt <- df_planetMood$VVIX 
ccf(yt, xt, lag=90, plot=TRUE, xlim=range(-90,-1))
ccf(yt, xt, lag=90, plot=FALSE, xlim=range(-90,-1))


# Pairs correlations in date groups
# install.packages("astsa")
library(astsa)
VIX <- df_planetMood$VIX
VVIX <- df_planetMood$VVIX
lag2.plot(VVIX, VIX, 
          max.lag = 15, 
          smooth = TRUE, 
          cex=0.2, pch=19, col=5, bgl='transparent', lwl=2, gg=T, box.col=gray(1))








#========================================================
# FPP3 examples
#========================================================

# cross-correlation and distribution
df_planetMood_ts %>%
  GGally::ggpairs(columns = (2:4))


# lag-plot by season
df_planetMood_ts %>% gg_lag(VIX, geom = "point")


# decomposition
dcmp <- df_planetMood_ts %>%
  model(stl = STL(VIX))

components(dcmp)

components(dcmp) %>%
  as_tsibble() %>%
  autoplot(VIX, colour="gray") +
  geom_line(aes(y=trend), colour = "#D55E00")

components(dcmp) %>% autoplot()

components(dcmp) %>%
  as_tsibble() %>%
  autoplot(VIX, colour="gray") +
  geom_line(aes(y=season_adjust), colour = "#D55E00")

df_planetMood_ts %>%
  model(
    STL(VIX ~ trend(window = 7) +
          season(window = "periodic"),
        robust = TRUE)) %>%
  components() %>%
  autoplot()





