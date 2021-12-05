# https://www.rdocumentation.org/packages/openSkies/versions/1.1.1
# https://cran.r-project.org/web/packages/openSkies/openSkies.pdf

library("openSkies")


# Obtain a list with information for all the flights that departed from Seville
# Airport on the 25th of July, 2019 between 9 AM and 11 AM, local time.


testDepartures <- getAirportDepartures(airport="LEZL", startTime="2019-07-25 09:00:00",
                     endTime="2019-07-25 11:00:00", timeZone="Europe/Madrid")

class(testDepartures)
class(testDepartures[[1]])



# Obtain a list with information for all the flights registered during the 16th
# of November, 2019 between 9 AM and 10 AM, London time.
flights <- getIntervalFlights(startTime="2019-11-16 09:00:00", 
                              endTime="2019-11-16 10:00:00", timeZone="Europe/London")

# Count the number of registered flights.

if(interactive()){
  length(flights)
}
