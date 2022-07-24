# ================================
# Extract stock data
# ================================

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

# we extract close price and "day volatility" for all securities.
# This requires to calculate new measure and the gather to create new variables named "*hlVOL" to distinguish from general close price
# Day volatility considers drift and is based in Rogers and Satchell (1991) and Rogers, Satchell and Yoon (1994) (*)
# (*): see paper: https://core.ac.uk/download/pdf/52391988.pdf

stocksData <- stocksData %>% 
  mutate (dayVolatility = (log(high) - log(open)) * (log(close) - log(low)) + (log(low) - log(open)) * (log(low) - log(close))) %>% 
  select(date, symbol, close, dayVolatility) %>% 
  gather(key="measure", value="value", -date, -symbol) %>% 
  mutate(symbol = if_else(measure == "dayVolatility", paste0(symbol,"hlVOL"), symbol)) %>% 
  select(-measure) %>%
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


