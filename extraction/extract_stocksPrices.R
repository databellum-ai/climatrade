# ================================
# Extract stock data
# ================================

library(tidyquant)
library(tidyverse)

#============ Prices parameters to extract (Yahoo! Finance):
print("Loading seed")
head(seedFeatures_df)
chosenTickers <- seedFeatures_df %>% filter(source %in% c("stocks")) %>% select(variable)
chosenTickers <- chosenTickers$variable
chosenTickers

print("Extracting stocks")

endDateTicker <- today()
startingDateTicker <- "1900-01-01"


stocksData <- tq_get(chosenTickers,
                 from = startingDateTicker,
                 to = endDateTicker,
                 get = "stock.prices")
# we extract close price
stocksData <- stocksData %>%
  select(date, symbol, value = close) %>%
  group_by(date, symbol) %>%
  summarise(value = mean(value)) %>%
  arrange(desc(date))
stocksData_ts <- stocksData %>%
  spread(key=symbol, value = value) %>% mutate(countryCode = NA) %>%
  arrange(desc(date))

head(stocksData)
head(stocksData_ts)

# Save to RDS
# saveRDS(stocksData, "data/data_stocks_tidy.rds")
saveRDS(stocksData_ts, "data/data_stocks_ts.rds")

print("Stocks values extraction process FINISHED")



# ------------------------------------------------
# ------------------------------------------------
# ------------------------------------------------
# ------------------------------------------------

# library(tidyquant)
# library(tidyverse)
# library(zoo)
# 
# chosenTickers <- c("^VIX", "^VVIX", "GC=F")
# 
# # Related to HISTORIC VOLATILITY (S&P500_SMA, S&P500_EMA, S&P500_HighLow_SMA, S&P500_HighLow_EMA) (https://rpubs.com/antonioh8/finModelingQuiz4_a)
# 
# tq_transmute_fun_options()
# 
# 
# endDateTicker <- today()
# startingDateTicker <- "1900-01-01"
# stocksData <- 
#   tq_get(chosenTickers, from = startingDateTicker, to = endDateTicker, get = "stock.prices") %>% 
#   mutate(close = na_locf(stocksData$close)) %>% 
#   tq_mutate(select = close, mutate_fun = EMA, n = 50, col_rename = "MA") %>% 
#   tq_mutate(select = close, mutate_fun = volatility, calc="close", col_rename = "V") %>% 
#   mutate(HL = (high - low))
# stocksData %>% arrange(desc(date))
# 
# 
# 
# # Draw Graph 2x2
# matplot(stocksData$V, axes=FALSE, type=c('l'), col = c('blue'), main = names(stocksData$value))
# axis(2) # show y axis
# axis(1, at=seq_along(1:nrow(stocksData)),
#      labels=stocksData$date, las=2)



