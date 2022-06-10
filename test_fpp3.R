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

# PlanetMood dataset
df_planetMood <- readRDS("data/df_planetMood.rds")  # Dataset ready for analysis 
names(df_planetMood)
df_planetMood_ts <- df_planetMood %>% as_tsibble(index = date)
names(df_planetMood_ts)
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



# FPP3 examples

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








