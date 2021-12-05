# PTE: almacenar resultados
# PTE: determinar una fecha razonable de inicio
# PTE: cálculo incremental de fechas basado en .RDS acumulado

# https://www.rdocumentation.org/packages/openSkies/versions/1.1.1
# https://cran.r-project.org/web/packages/openSkies/openSkies.pdf

library(tidyverse)
library(openSkies)
library(lubridate)

startDate <- "2018-01-30"
initialDateAvailable <- ymd(startDate, tz = "UTC")
currentDate <-as.POSIXct(Sys.Date()-1)
allDates <- seq(initialDateAvailable, currentDate, by="days")
allDates

coef_00to02_UTCtoTotal <- 0.074681009  # We use this coefficient to estimate full-day flight based on 2 hours period from 00 to 02 UTC. This was calculated using a linear regression approach from all 2-hours periods within a representative day

for (i in c(length(allDates):1)) {
  dateStart <- allDates[i]
  dateEnd <- dateStart + hours(23) + minutes(59) + seconds(59)  
  dateEnd_sample <- dateStart + hours(02) + minutes(00) + seconds(00)  # API restricted to 2 hours for total flights
  print("-------------")
  print(paste("DATE:",dateStart))
  numFlightsWorld <- getIntervalFlights(startTime=dateStart, 
                                        endTime=dateEnd_sample, timeZone="UTC")
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
  }
}

