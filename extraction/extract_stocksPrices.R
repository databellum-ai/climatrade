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

# we distinguish close price
stocksClosePrice <- stocksData %>% select(date, symbol, value = close) %>% arrange(desc(date))
# we distinguish volume values and add a suffix to asset name
stocksVolume <- stocksData %>% mutate(symbol=paste0(symbol,".vol")) %>% select(date,symbol, value=volume) %>% arrange(desc(date))
# joining close prices and volumes (with suffix ".vol" in the asset name)
stocksData <- rbind(stocksClosePrice, stocksVolume)
stocksData_ts <- stocksData %>% spread(key=symbol, value = value) %>% arrange(desc(date))

head(stocksData)
head(stocksData_ts)


# Save to RDS
saveRDS(stocksData, "data/stocksData_tidy.rds")
saveRDS(stocksData_ts, "data/stocksData_ts.rds")

print("Stock prices and volumes process FINISHED")
