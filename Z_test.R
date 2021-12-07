

head(historicDailyFlights)
head(tmpDailyFlights)

nrow(tmpDailyFlights)
if (nrow(tmpDailyFlights) != 0) {
  historicDailyFlights <- tmpDailyFlights %>% 
  group_by(date, countryCode) %>% 
  summarise(airportDepartures = mean(airportDepartures), worldFlights = mean(worldFlights)) %>% 
  rbind(historicDailyFlights)
}


head(historicDailyFlights)

