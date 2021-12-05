# PTE: OJO: los datos empiezan en 2016 y su volumen va creciendo con el número de voluntarios
# ...es mejor usar un ratio (p.e. vuelos desde NYC ó Shangai respecto al total)


# https://www.rdocumentation.org/packages/openSkies/versions/1.1.1
# https://cran.r-project.org/web/packages/openSkies/openSkies.pdf


library("openSkies")

# Obtain a list with information for all the flights that departed from Seville
# Airport on the 25th of July, 2019 between 9 AM and 11 AM, local time.
testDepartures <- getAirportDepartures(airport="KJFK", startTime="2018-01-30 19:00:00",
                                   endTime="2018-01-30 20:00:00", timeZone="Europe/London")
testArrivals <- getAirportArrivals(airport="KJFK", startTime="2018-01-30 19:00:00",
                     endTime="2018-01-30 20:00:00", timeZone="Europe/London")
length(testDepartures)
length(testArrivals)
# Operations
operations <- length(testDepartures) + length(testArrivals)
operations
testDepartures

# Obtain a list with information for all the flights registered during the 16th
# of November, 2019 between 9 AM and 10 AM, London time.
flights <- getIntervalFlights(startTime="2018-01-30 19:00:00", 
                              endTime="2018-01-30 20:00:00", timeZone="Europe/London")
# Count the number of registered flights.
length(flights)
worldFlights
