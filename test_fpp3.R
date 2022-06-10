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

# seasonality
df_planetMood_ts %>% gg_season(VIX, period = "week", labels = "both") +
  labs(y = "$ (millions)",
       title = "Seasonal plot: Antidiabetic drug sales")
# cross-correlation and distribution
df_planetMood_ts %>%
  GGally::ggpairs(columns = (2:4))
# lag-plot by season
df_planetMood_ts %>% gg_lag(Gold, geom = "point")

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







us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade") %>%
  select(-Series_ID)
autoplot(us_retail_employment, Employed)

dcmp <- us_retail_employment %>%
  model(stl = STL(Employed))
components(dcmp)



PBS %>%
  filter(ATC2 == "A10") %>%
  select(Month, Concession, Type, Cost) %>%
  summarise(TotalC = sum(Cost)) %>%
  mutate(Cost = TotalC / 1e6) -> a10

visitors <- tourism %>%
  group_by(State) %>%
  summarise(Trips = sum(Trips))
visitors %>%
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(vars(State), scales = "free_y") +
  labs(title = "Australian domestic tourism",
       y= "Overnight trips ('000)")
visitors %>%
  pivot_wider(values_from=Trips, names_from=State) %>%
  GGally::ggpairs(columns = 2:9)

recent_production <- aus_production %>%
  filter(year(Quarter) >= 2000)
recent_production %>%
  gg_lag(Beer, geom = "point") +
  labs(x = "lag(Beer, k)")

