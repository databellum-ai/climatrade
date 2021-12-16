# PTE: Revisar el warning
# PTE: prueba de volumen de symbols


# ================================
# Extract stock data
# ================================

library(tidyverse)
library(tidyquant)

print("Extracting stock prices and volumes")

endDateTicker <- today()
startingDateTicker <- "1900-01-01"



# ~~~~~~~~~~~~~~
tq_get(c("^VIX"), 
       from = "2021-12-14", 
       to = "2021-12-16", 
       get = "stock.prices")


# ~~~~~~~~~~~~~~




stocksData <- tq_get(chosenTickers,
                 from = startingDateTicker,
                 to = endDateTicker,
                 get = "stock.prices")

# we separate close prices and spread for time series format
stocksClosePrice_tidy <- stocksData %>% 
  select(date,symbol, close)
stocksClosePrice_ts <- stocksClosePrice_tidy %>% 
  spread(key=symbol, value = close)
# we separate close prices and spread for time series format
stocksVolume_tidy <- stocksData %>% 
  select(date,symbol, volume)
stocksVolume_ts <- stocksVolume_tidy %>% 
  spread(key=symbol, value = volume)
head(stocksClosePrice_tidy)
head(stocksVolume_tidy)
head(stocksClosePrice_ts)
head(stocksVolume_ts)

# Save to RDS
saveRDS(stocksClosePrice_ts, "data/stocksClosePrice_tidy.rds")
saveRDS(stocksClosePrice_ts, "data/stocksClosePrice_ts.rds")
saveRDS(stocksVolume_ts, "data/stocksVolume_tidy.rds")
saveRDS(stocksVolume_ts, "data/stocksVolume_ts.rds")

print("Stock prices and volumes process FINISHED")
