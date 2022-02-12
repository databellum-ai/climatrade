# ================================
# EXTRACT AIR TRAFFIC DATA FROM THE OPENSKY NETWORK
# ================================

# https://www.rdocumentation.org/packages/openSkies/versions/1.1.1
# https://cran.r-project.org/web/packages/openSkies/openSkies.pdf

library(tidyverse)
library(openSkies)
library(lubridate)

startDate <- "2016-01-01"
startDate <- "2022-01-01"
yesterdayRefDate <- Sys.Date()-1

# ================================
# STEP 1: Determine dates we need to collect and check if incremental or full process required
# ================================
# Determine what dates we'll process:
unProcessedDates <- NULL
allPossibleDates <- seq(ymd(startDate, tz = "UTC"), ymd(yesterdayRefDate, tz = "UTC"), by="days")
allPossibleDates
# read already available data to ensure calculation only of delta. At the end we'll consolidate
if (file.exists("data/data_airTraffic_ts.rds")) {
  historicDailyFlights <- readRDS("data/data_airTraffic_ts.rds")
  # get a list of dates appearing in the website but not in our historic record and use it for further process
  existingDates <- historicDailyFlights %>% group_by(date) %>% summarize()
  unProcessedDates <- allPossibleDates[!(allPossibleDates %in% as.POSIXct(existingDates$date))]
} else {
  # in case there is no history of data, we must process all available dates
  historicDailyFlights <- data.frame()
  unProcessedDates <- allPossibleDates
}
print("Dates to process:")
print(unProcessedDates)

if (length(unProcessedDates) > 0) {
  # ================================
  # STEP 2: Obtain flights for pending dates on specified list of airports
  # ================================
  coef_00to02_UTCtoTotal <- 0.074681009  # We use this coefficient to estimate full-day flight based on 2 hours period from 00 to 02 UTC. This was calculated using a linear regression approach from all 2-hours periods within a representative day
  
  # Prepare empty dataframe to store results
  tmpDailyFlights <- data.frame()
  for (i in c(length(unProcessedDates):1)) {
    dateStart <- unProcessedDates[i]
    dateEnd <- dateStart + hours(23) + minutes(59) + seconds(59)  
    dateEnd_sample <- dateStart + hours(02) + minutes(00) + seconds(00)  # API restricted to 2 hours for total flights
    numFlightsWorld <- getIntervalFlights(startTime=dateStart, endTime=dateEnd_sample, timeZone="UTC")  # OpenSkies API
    numFlightsWorld <- round(length(numFlightsWorld) / coef_00to02_UTCtoTotal)  # Flights estimated full day
    print(paste0("World flights on ",dateStart, ": ", numFlightsWorld))
    tmpRecord <- data.frame(
      date = as.Date(dateStart), 
      countryCode = NA, 
      worldFlights = numFlightsWorld)
    # We store only y flights have values
    if (numFlightsWorld != 0) {
      tmpDailyFlights <- rbind(tmpDailyFlights, tmpRecord)
    }
  }
  
  head(tmpDailyFlights)
  
  # We add obtained records to historical (if something obtained)
  if (nrow(tmpDailyFlights) != 0) {
    historicDailyFlights <- tmpDailyFlights %>% rbind(historicDailyFlights)
  }
  
  # ================================
  # SAVE
  # ================================
  # # Save dataset
  saveRDS(as_tibble(historicDailyFlights),"data/data_airTraffic_ts.rds")
  
}

