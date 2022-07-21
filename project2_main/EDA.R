# EDA of generated forecasts:
data_all_d <- readRDS("project2_main/recommendationsNN_all.rds")

# -------------------------------------------
# Hyperparameter: calendarHorizon (= 14 days)
# -------------------------------------------
# we have generated forecast examples with horizons (days to schudule in the future) of 7, 14 and 21 days
# looking at the scattered charts it seems there are good high number of Success cases, as well as high Earnings using 21 days
# however, it still requires a to be refined later by a regression model
# grouped 2D chart:
dt1 <- data_all_d %>% 
  group_by(horizon, txnLength) %>% 
  summarise(EarningsPerc = sum(earningsPercent), SuccessPerc = mean(success)) %>% 
  arrange(desc(SuccessPerc)) 
dt1 %>% 
  ggplot(aes(txnLength, SuccessPerc, color = as.factor(horizon), size = EarningsPerc)) +
  geom_point()
# grouped 3D chart:
# https://plotly.com/r/3d-scatter-plots/
library(plotly)
plot_ly(dt1, x = ~horizon, y = ~txnLength, z = ~SuccessPerc, color = ~EarningsPerc, colors = c('yellow', 'red')) %>% add_markers() %>% layout(scene = list(xaxis = list(title = 'Horizon used'),
                                   yaxis = list(title = '# days open'),
                                 zaxis = list(title = '% Success')))

head(data_all_d)



# -------------------------------------------
# Show VIX components (year and week seasonalities)
# -------------------------------------------
as_tsibble(dataUptodate, index = date) %>%
  model(
    STL(VIX ~ trend(window = 7) + season(window = "periodic"), robust = TRUE)) %>% 
  components() %>% 
  autoplot()