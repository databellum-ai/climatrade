# EDA of generated forecasts:
# manage accumulated
# data_7d <- cbind(readRDS("data/test_accuracies_7d.rds"), calcHorizon = 7)
# data_14d <- cbind(readRDS("data/test_accuracies_14d.rds"), calcHorizon = 14)
# data_21d <- cbind(readRDS("data/test_accuracies_21d.rds"), calcHorizon = 21)
# data_all_d <- rbind(data_7d, data_14d, data_21d)
# saveRDS(data_all_d,"data/test_accuracies_all_d.rds")
data_all_d <- readRDS("data/test_accuracies_all_d.rds")
names(data_all_d)
head(data_all_d)
data_all_d

# -------------------------------------------
# Hyperparameter: calendarHorizon (= 21 days)
# -------------------------------------------
# we have generated forecast examples with horizons (days to schudule in the future) of 7, 14 and 21 days
# looking at the scattered charts it seems there are good high number of Success cases, as well as high Earnings using 21 days
# however, it still requires a to be refined later by a regression model
# all available observations
data_all_d %>% 
  ggplot(aes(txnLength, earningsPercent, color = as.factor(calcHorizon))) +
  geom_point()
# grouped 2D chart:
data_all_d <- data_all_d %>% 
  group_by(calcHorizon, txnLength) %>% 
  summarise(EarningsPerc = sum(earningsPercent), SuccessPerc = mean(success)) %>% 
  arrange(desc(SuccessPerc))
data_all_d %>% 
  ggplot(aes(txnLength, SuccessPerc, color = as.factor(calcHorizon), size = EarningsPerc)) +
  geom_point()
# grouped 3D chart:
# https://plotly.com/r/3d-scatter-plots/
library(plotly)
plot_ly(data_all_d, x = ~calcHorizon, y = ~txnLength, z = ~SuccessPerc, color = ~EarningsPerc, colors = c('yellow', 'red')) %>% add_markers() %>% layout(scene = list(xaxis = list(title = 'calcHorizon'),
                                   yaxis = list(title = 'txnLength'),
                                 zaxis = list(title = 'SuccessPerc')))



data_all_d
recommendations <- data_all_d



