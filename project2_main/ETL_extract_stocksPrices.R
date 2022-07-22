# ================================
# Extract stock data
# ================================

library(tidyquant)
library(lubridate)

chosenTickers <- c("^VIX", "^VVIX", "^VIX3M", "^VXN", "^GVZ")

print("Extracting stocks")

endDateTicker <- today()
startingDateTicker <- "2015-01-01"
# read datafrom source
stocksData <- tq_get(chosenTickers,
                     from = startingDateTicker,
                     to = endDateTicker,
                     get = "stock.prices")
# format dataframe
stocksData <- stocksData %>% 
  select(date, symbol, close) %>% 
  gather(key="measure", value="value", -date, -symbol) %>% 
  select(-measure) %>%
  group_by(date, symbol) %>%
  summarise(value = mean(value)) %>%
  arrange(desc(date)) %>%
  spread(key=symbol, value = value) %>%
  arrange(desc(date))

# Ensure there are records for all possible dates, even if they have no value o any feature
allAbsoluteDates <- as_tibble(as.Date(seq(ymd(as_date(startingDateTicker), tz = "UTC"), as.POSIXct(Sys.Date()), by="days")))
colnames(allAbsoluteDates) <- c("date")
stocksData <- stocksData %>% 
  right_join(allAbsoluteDates, by = "date")
stocksData <- stocksData %>% arrange(desc(date)) %>% 
  mutate(
    month = month(date),
    dayInMonth = day(date),
    WkDay = wday(date), 
    YrWeek = week(ymd(date))
  )

head(stocksData)
stocksData %>% arrange(desc(date))

# imputation, remove outliers

# Function to detect start and end of time serie values and apply interpolation to fill missing values (NAs)
# Missing Values Very recent (nor yet extracted) or very old (before historic availability) will remain NA 
# On imputation function: https://cran.r-project.org/web/packages/imputeTS/imputeTS.pdf
imputeFeatureWithinExistingInterval <- function(tsValues) {
  # start <- which.min(is.na(tsValues))  # In case we want to do imputation only until most recent KNOWN value and not beyond
  start <- 1
  end <- length(tsValues) - which.min(is.na(tsValues[length(tsValues):1]))
  imputedWithinExistingInterval <- na_interpolation(tsValues[start:end])
  tsValues[start:end] <- imputedWithinExistingInterval
  tsValues
}
# Function to remove outliers based in 1.5 times distance between percentiles 1 and 99 (pretty conservative)
# For these outliers, value is moved to NA, and it will be in next step imputated (linearly)
removeOutliers <- function(tsValues) {
  Q <- quantile(tsValues, probs=c(.001, .999), na.rm = TRUE)
  iqr <- IQR(tsValues, na.rm = TRUE)
  low <- Q[1]-1.5*iqr # Lower Range
  up <-  Q[2]+1.5*iqr # Upper Range
  tsValues <- ifelse(!(between(tsValues,low,up)),NA,tsValues)
  tsValues
}
# Remove columns with <= 2 valid values to avoid imputation issues
empty_columns <- sapply(stocksData, function(x) sum(!is.na(x)) <= 2)
stocksData <- stocksData[, !empty_columns]
# Temporarily remove "date" column to call functions that remove outliers and make massive imputation
tmp_df_noDate <- stocksData[,!(colnames(stocksData) == "date")]
# Perform outliers removal
tmp_df_noDate <- as_tibble(apply(tmp_df_noDate, MARGIN=2, FUN=removeOutliers))
# Perform massive imputation to numeric time series (features)
tmp_df_noDate <- as_tibble(apply(tmp_df_noDate, MARGIN=2, FUN=imputeFeatureWithinExistingInterval))
stocksData <- cbind(date = stocksData$date, as.data.frame(tmp_df_noDate)) # Add again "date" to processed file


# Save to RDS
saveRDS(stocksData, "project2_main/data_stocks.rds")

print("Closing values extraction process FINISHED")
