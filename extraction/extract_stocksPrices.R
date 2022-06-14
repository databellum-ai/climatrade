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



# !! CALCULAR MA, HL Y V... STOCK A STOCK!
stocksData2 <- 
  tq_get(chosenTickers, from = startingDateTicker, to = endDateTicker, get = "stock.prices") %>% 
  mutate(close = na_locf(close)) %>% 
  tq_mutate(select = close, mutate_fun = EMA, n = 50, col_rename = "MA") %>% 
  tq_mutate(select = close, mutate_fun = volatility, calc="close", col_rename = "V") %>% 
  mutate(HL = (high - low)) %>% 
  select(date, symbol, close, MA, HL, V) %>% 
  arrange(desc(date))

# stocksData <- tq_get(chosenTickers,
#                  from = startingDateTicker,
#                  to = endDateTicker,
#                  get = "stock.prices")
# # we extract close price
# stocksData <- stocksData %>% 
#   select(date, symbol, value = close) %>% 
#   group_by(date, symbol) %>% 
#   summarise(value = mean(value)) %>% 
#   arrange(desc(date))
# stocksData_ts <- stocksData %>% 
#   spread(key=symbol, value = value) %>% mutate(countryCode = NA) %>% 
#   arrange(desc(date))

head(stocksData)
head(stocksData_ts)

# Save to RDS
# saveRDS(stocksData, "data/data_stocks_tidy.rds")
saveRDS(stocksData_ts, "data/data_stocks_ts.rds")

print("Stocks values extraction process FINISHED")





# ------------------------
# ------------------------

library(tidyquant)
library(tidyverse)
library(zoo)

chosenTickers <- c("^VIX", "^VVIX", "GC=F")

# https://www.investopedia.com/5-volatility-indicators-are-warnings-sign-to-the-markets-4584057
# INDICATORS WE NEED:
#   "^GSPC" -> S&P 500
#   "ZN=F" -> 10-Year Treasury Note Futures
#   "^KS200" -> KOSPI
#   VIX FUTURES -> ??
# MEASURES WE NEED:
#   Volatility
#     VOLATILITY FUNCTION IN R: 
#     TTR USED WITHIN tq_get():
#       https://www.rdocumentation.org/packages/TTR/versions/0.24.3/topics/volatility
#       https://cran.r-project.org/web/packages/tidyquant/vignettes/TQ02-quant-integrations-in-tidyquant.html
#   50 days EMA
#     TTR WITHIN tq_get(): https://www.rdocumentation.org/packages/TTR/versions/0.24.3/topics/volatility
#   Intraday High-Low variation:
#     high-close direntamente?
#     TTR WITHIN tq_get()?
# ABOUT VKOSPI:
# https://www.investing.com/indices/kospi-volatility
# https://www.jstor.org/stable/41739216
tq_transmute_fun_options()


endDateTicker <- today()
startingDateTicker <- "1900-01-01"
stocksData <- 
  tq_get(chosenTickers, from = startingDateTicker, to = endDateTicker, get = "stock.prices") %>% 
  mutate(close = na_locf(stocksData$close)) %>% 
  tq_mutate(select = close, mutate_fun = EMA, n = 50, col_rename = "MA") %>% 
  tq_mutate(select = close, mutate_fun = volatility, calc="close", col_rename = "V") %>% 
  mutate(HL = (high - low))
stocksData %>% arrange(desc(date))



# Draw Graph 2x2
matplot(stocksData$V, axes=FALSE, type=c('l'), col = c('blue'), main = names(stocksData$value))
axis(2) # show y axis
axis(1, at=seq_along(1:nrow(stocksData)),
     labels=stocksData$date, las=2)



