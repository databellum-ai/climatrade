source("project2_main/initialize.R")

# EDA of generated forecasts:
data_all_d <- readRDS("project2_main/recommendationsNN_all.rds")

# analyze recommendations
data_all_d %>% 
  # filter(length>=1904) %>%    # as_date("2015-01-01") + 2616 = "2022-03-20"
  filter(
    horizon == txnLength & 
      regressors == "VX+C2"
  ) %>%
  group_by(regressors, action, volatility = (VIX_txn>30), horizon, txnLength) %>%
  summarise(n = n(), Mean_TxnEarning = mean(earningsPercent), Mean_success = mean(success)) %>% 
  arrange(desc(horizon), (txnLength)) %>% 
  arrange(desc(Mean_success))


# grouped 2D chart:
dt1 <- data_all_d %>% 
  group_by(horizon, txnLength) %>% 
  summarise(EarningsPerc = sum(earningsPercent), SuccessPerc = mean(success)) %>% 
  arrange(desc(SuccessPerc)) 
dt1 %>% 
  ggplot(aes(txnLength, SuccessPerc, color = as.factor(horizon), size = EarningsPerc)) +
  geom_point()

#test2



# grouped 3D chart:
# https://plotly.com/r/3d-scatter-plots/
library(plotly)
plot_ly(dt1, x = ~horizon, y = ~txnLength, z = ~SuccessPerc, color = ~EarningsPerc, colors = c('yellow', 'red')) %>% add_markers() %>% layout(scene = list(xaxis = list(title = 'Horizon used'),
                                   yaxis = list(title = '# days open'),
                                 zaxis = list(title = '% Success')))

head(dt1)





# Show VIX components (year and week seasonalities)
dataUptodate <- readRDS("project2_main/dataUptodate.rds")
as_tsibble(dataUptodate, index = date) %>%
  model(
    STL(VIX ~ trend(window = 7) + season(window = "periodic"), robust = TRUE)) %>% 
  components() %>% 
  autoplot()




