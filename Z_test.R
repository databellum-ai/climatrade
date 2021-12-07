startDate <- "2015-12-31"
yesterdayRefDate <- Sys.Date()-1

unProcessedDates <- NULL
allPossibleDates <- seq(ymd(startDate, tz = "UTC"), ymd(yesterdayRefDate, tz = "UTC"), by="days")
allPossibleDates


dateStart <- allPossibleDates[1]
dateEnd <- dateStart + hours(23) + minutes(59) + seconds(59)  
dateEnd_sample <- dateStart + hours(02) + minutes(00) + seconds(00)  # API restricted to 2 hours for total flights

dateStart
dateEnd 
dateEnd_sample

length(getIntervalFlights(startTime=dateStart, endTime=dateEnd_sample, timeZone="UTC"))

