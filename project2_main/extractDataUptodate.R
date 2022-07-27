
source("./project2_main/initialize.R")

# ------------------------------------------------------
# extract daily data from live sources from history until last close

extractDataUptodate <- function() {
  selectedVbles <- c("^VIX", "^VVIX", "^VIX3M", "^VXN", "^GVZ")
  endDateTicker <- as.character(today())
  startingDateTicker <- "2015-01-01"  
  
  print("Extracting stocks")
  # read datafrom source

  stocksData <- tq_get(selectedVbles,
                       from = startingDateTicker,
                       to = endDateTicker,
                       get = "stock.prices"
                       , verbose = TRUE)

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
      wkDay = wday(date), 
      yrWeek = week(ymd(date))
    )
  
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
  
  # PlanetMood dataset
  stocksData <- stocksData %>% 
    select(date, 
           selectedVbles, 
           month, dayInMonth, wkDay, yrWeek) %>% 
    arrange(date)
  names(stocksData) <- c("date", "VIX", "VVIX", "VIX3M", "VIXNsdq", "GoldVlty", "month", "dayInMonth", "wkDay", "yrWeek")
  return(stocksData)
}


transformation <- ""

dataUptodate <- extractDataUptodate()
head(dataUptodate)
tail(dataUptodate)
# all recommendations generated are consolidated in a RDS for further analysis
saveRDS(dataUptodate,"./project2_main/dataUptodate.rds") #  save last available fresh daily data

