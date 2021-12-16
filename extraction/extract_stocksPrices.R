# PTE: revisar el warning en TIDYQUANT
# PTE: probar paquete QUANTMOD + ver si vale para "^VIX" ó "EURUSD=X" 
# PTE: prueba de volumen de symbols


# ================================
# Extract stock data
# ================================

library(tidyquant)
library(tidyverse)

print("Extracting stock prices and volumes")

endDateTicker <- today()
startingDateTicker <- "1900-01-01"








# ~~~~~~~~~~~~~~
# PROBLEMA DE LOS WARNINGS
# https://www.giters.com/business-science/tidyquant/issues/202
tq_get(c('^VIX'), 
       from = '2021-12-14', 
       to = '2021-12-16', 
       get = 'stock.prices')



# PACKAGE QUANTMOD
# https://bookdown.org/kochiuyu/Technical-Analysis-with-R/downloading-data.html
# https://cran.r-project.org/web/packages/quantmod/quantmod.pdf
test <- getSymbols("GOLD;SPY", from="1997-12-31", src='yahoo') 
head(test)
getSymbols("EURUSD=")
barChart(EURUSD=)

install.packages("quantmod")
library(quantmod)
getSymbols("GOLD")
getSymbols("AAPL", from='1980-01-01',to='2021-12-16')
getSymbols(AAPL)


tickers <- c("^IBEX", "NDAQ", "EURUSD=X", "GOLD", "^VIX", "AAPL", "NFLX", "AMZN", "AAIC", "MSFT", "AAN")
tickers <- c('AXP','CAT')
getSymbols(tickers, from = '2018-03-01')
dfNew <- data.frame(Company = character(),
                    Date=as.Date(character()),
                    Close=numeric(),
                    Volume=numeric(), 
                    stringsAsFactors=FALSE) 
for(i in 1:length(tickers)){
  dfSym <- cbind(Company=rep(tickers[i],nrow(get(tickers[i]))),fortify.zoo(get(tickers[i])[,5]))
  names(dfSym)[2:3] <- c('Date','Volume')
  dfNew <- rbind(dfNew,dfSym)
}
head(dfNew)
unique(dfNew$Company)

# ~~~~~~~~~~~~~~











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
