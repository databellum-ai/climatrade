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
stocksData <- stocksData %>% select(date, symbol, value = close) %>% group_by(date, symbol) %>% summarise(value = mean(value)) %>% arrange(desc(date))

stocksData_ts <- stocksData %>% 
  spread(key=symbol, value = value) %>% mutate(countryCode = NA) %>% 
  arrange(desc(date))

head(stocksData)
head(stocksData_ts)

# Save to RDS
# saveRDS(stocksData, "data/data_stocks_tidy.rds")
saveRDS(stocksData_ts, "data/data_stocks_ts.rds")

print("Stocks values extraction process FINISHED")

