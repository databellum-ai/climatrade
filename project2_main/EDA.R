# EDA of generated forecasts:
data_all_d <- readRDS("project2_main/recommendationsNN_all.rds")

# -------------------------------------------
# Hyperparameter: calendarHorizon (= 14 days)
# -------------------------------------------
# we have generated forecast examples with horizons (days to schudule in the future) of 7, 14 and 21 days
# looking at the scattered charts it seems there are good high number of Success cases, as well as high Earnings using 21 days
# however, it still requires a to be refined later by a regression model
# all available observations
data_all_d %>% 
  ggplot(aes(txnLength, earningsPercent, color = as.factor(horizon))) +
  geom_point()
# grouped 2D chart:
data_all_d <- data_all_d %>% 
  group_by(horizon, txnLength) %>% 
  summarise(EarningsPerc = sum(earningsPercent), SuccessPerc = mean(success)) %>% 
  arrange(desc(SuccessPerc))
data_all_d %>% 
  ggplot(aes(txnLength, SuccessPerc, color = as.factor(horizon), size = EarningsPerc)) +
  geom_point()
# grouped 3D chart:
# https://plotly.com/r/3d-scatter-plots/
library(plotly)
plot_ly(data_all_d, x = ~horizon, y = ~txnLength, z = ~SuccessPerc, color = ~EarningsPerc, colors = c('yellow', 'red')) %>% add_markers() %>% layout(scene = list(xaxis = list(title = 'Horizon used'),
                                   yaxis = list(title = '# days open'),
                                 zaxis = list(title = '% Success')))

head(data_all_d)




