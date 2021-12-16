# PTE: prueba de volumen de symbols


# ================================
# Extract stock data
# ================================

library(tidyquant)
library(tidyverse)

print("Extracting stock prices and volumes")

endDateTicker <- today()
startingDateTicker <- "1900-01-01"

stocksData <- tq_get(chosenTickers,
                 from = startingDateTicker,
                 to = endDateTicker,
                 get = "stock.prices")

# we separate close prices and spread for time series format
stocksClosePrice_tidy <- stocksData %>% 
  select(date,symbol, close) %>% arrange(desc(date))
stocksClosePrice_ts <- stocksClosePrice_tidy %>% 
  spread(key=symbol, value = close) %>% arrange(desc(date))
# we separate close prices and spread for time series format
stocksVolume_tidy <- stocksData %>% 
  select(date,symbol, volume) %>% arrange(desc(date))
stocksVolume_ts <- stocksVolume_tidy %>% 
  spread(key=symbol, value = volume) %>% arrange(desc(date))
head(stocksClosePrice_tidy)
head(stocksClosePrice_ts)
head(stocksVolume_tidy)
head(stocksVolume_ts)

# Save to RDS
saveRDS(stocksClosePrice_tidy, "data/stocksClosePrice_tidy.rds")
saveRDS(stocksClosePrice_ts, "data/stocksClosePrice_ts.rds")
saveRDS(stocksVolume_tidy, "data/stocksVolume_tidy.rds")
saveRDS(stocksVolume_ts, "data/stocksVolume_ts.rds")

print("Stock prices and volumes process FINISHED")
