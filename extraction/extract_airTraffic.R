# PTE: determinar una fecha razonable de inicio

# https://www.rdocumentation.org/packages/openSkies/versions/1.1.1
# https://cran.r-project.org/web/packages/openSkies/openSkies.pdf

library(tidyverse)
library(openSkies)
library(lubridate)

startDate <- "2021-11-01"

# ================================
# STEP 1: Determine dates we need to collect and check if incremental or full process required
# ================================
# Determine what dates we'll process:
unProcessedDates <- NULL
allPossibleDates <- seq(ymd(startDate, tz = "UTC"), as.POSIXct(Sys.Date()-1), by="days")
allPossibleDates
# read already available data to ensure calculation only of delta. At the end we'll consolidate
if (file.exists("data/data_airTraffic_ts.rds")) {
  historicDailyFlights <- readRDS("data/data_airTraffic_ts.rds")
  # get a list of dates appearing in the website but not in our historic record and use it for further process
  existingDates <- historicDailyFlights %>% group_by(date) %>% summarize()
  unProcessedDates <- allPossibleDates[!(allPossibleDates %in% as.POSIXct(existingDates$date))]
  # Compare list of requested countries with list of available in historical data
  sameCountriesList <- identical(sort(unique(countries)), sort(unique(historicDailyFlights$countryCode)))
} else {
  # in case there is no history of data, we must process all available dates
  historicDailyFlights <- data.frame()
  unProcessedDates <- allPossibleDates
}
# Data must be fully recalculated if countries exisitng and demanded are different
if (!sameCountriesList) {
  historicDailyFlights <- data.frame()
  unProcessedDates <- allPossibleDates
  print("NOTE data will be fully extracted since list of countries is different than last full run")
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
  # tmpDailyFlights <- data.frame(matrix(ncol = 5, nrow = 0))
  # colnames(tmpDailyFlights) <- c("date", "airportCode", "countryCode", "airportDepartures", "countryDepartures")
  # tmpDailyFlights
  # Call flights API for each day and airport
  for (i in c(length(unProcessedDates):1)) {
    dateStart <- unProcessedDates[i]
    dateEnd <- dateStart + hours(23) + minutes(59) + seconds(59)  
    dateEnd_sample <- dateStart + hours(02) + minutes(00) + seconds(00)  # API restricted to 2 hours for total flights
    print("-------------")
    print(paste("DATE:",dateStart))
    numFlightsWorld <- getIntervalFlights(startTime=dateStart, endTime=dateEnd_sample, timeZone="UTC")  # OpenSkies API
    numFlightsWorld <- round(length(numFlightsWorld) / coef_00to02_UTCtoTotal)  # Flights estimated full day
    print(paste0("World flights on ",dateStart, ": ", numFlightsWorld))
    for (j in c(1:length(airports))) {
      tmpDepartures <- getAirportDepartures(
        airport=airports[j],
        startTime=dateStart,
        endTime=dateEnd,
        timeZone="UTC")
      numDepartures <- length(tmpDepartures)
      print(paste0("Airport ",airports[j]," (",countries[j],") "," # departures: ",numDepartures))
      tmpRecord <- data.frame(
        date = as.Date(dateStart), 
        airportCode = airports[j], 
        countryCode = countries[j], 
        airportDepartures = numDepartures, 
        worldFlights = numFlightsWorld)
      # We store only y flights have values
      if (numDepartures != 0 & numFlightsWorld != 0) {
        tmpDailyFlights <- rbind(tmpDailyFlights, tmpRecord)
      }
    }
  }
  head(tmpDailyFlights)
  
  # We add obtained records to historical
  historicDailyFlights <- tmpDailyFlights %>% 
    group_by(date, countryCode) %>% summarise(airportDepartures = mean(airportDepartures), worldFlights = mean(worldFlights)) %>% 
    rbind(historicDailyFlights)
  head(historicDailyFlights)
  
  # ================================
  # SAVE
  # ================================
  # # Save dataset
  saveRDS(historicDailyFlights,"data/data_airTraffic_ts.rds")
  # # Save also countries and codes for further consolidation with other data
  geo_airTraffic <- historicDailyFlights %>% group_by(countryCode) %>% summarise() %>% arrange(countryCode)
  geo_airTraffic
  saveRDS(geo_airTraffic,"data/geo_airTraffic.rds")
}
