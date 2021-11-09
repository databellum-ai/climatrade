# -----------------------------------------------------------------
# -----------------------------------------------------------------
# Data extraction from miscelaneous sources
# -----------------------------------------------------------------
# -----------------------------------------------------------------


# =========================================
# Extract music trends from SPOTIFY
# =========================================
# Parameters music extraction:
numTopTracks <- 3 # how many tracks we extract per day/week
lotSize <- 10  # dates processed in a run
#
# Each call download only a number of dates. We need to iterate
for (i in c(1:40)) {
  print(paste("===================== ITERATION OF EXTRACT:", i))
  source("extraction/extract_musicSPOTIFY.R")
}






# =========================================
# Extract weather from NOAA
# =========================================
year_from_NOAA <- 1989
refreshAlsoStations <- FALSE
from_y <- year_from_NOAA # initial year (parameter)
to_y <- as.character(year(Sys.Date())) # current year
c_radius <- 80 # radius where stations must be around city
c_limit <- 10 # max. num. of stations considered
nUsedStations <- 4 # max. num. stations we'll consider per city
# Define our target cities to look for near stations near them ("10 ciudades que dirigen la economía mundial": https://cincodias.elpais.com/cincodias/2007/06/13/sentidos/1181701636_850215.html)
cities <- c("NewYork", "Oviedo", "Paris", "HongKong", "London", "Beijing", "Madrid", "Albacete", "Tokyo")
addresses <- c("New York City, US", "Oviedo, Spain", "Paris, France", "Hong Kong", "London, England", "Beijing, China", "Madrid, Spain", "Albacete, Spain", "Tokyo, Japan")
relevantCities <- data.frame(name = cities, addr = addresses)
relevantCities <- relevantCities %>% 
  geocode(addr) %>% 
  select(id = name, latitude = lat, longitude = long)
relevantCities
#
source("extraction/extract_weatherNOAA.R")



# =========================================
# Extract from FIFA Ranking
# =========================================
#
source("extraction/extract_rankingFIFA.R")



# =========================================
# Extract stock prices from Yahoo Finance
# =========================================
# Prices parameters:
chosenTickers = c("AAPL", "NFLX", "AMZN", "AAIC", "MSFT", "AAN")
endDateTicker = as.Date(today())
startingDateTicker = endDateTicker - 365 # also as... "2021-01-01"
#
source("extraction/extract_StockPrices.R")

# =========================================
# Extract searches from Google Trends
# =========================================
# Searches parameters:
search_concept_gral <- "Banks"
search_concepts <- c("unicaja", "bbva", "santander")
search_places = c("ES", "ES", "ES") # ("" for all)
search_period <- "today 12-m" # Examples: "all" for all (since 1jan2004 monthly), "today+5-y" for last five years (default, weekly), "today 12-m" for 12 month from today (weekly), "today 3-m" for 3 months from today (daily), "now 7-d" for last week (hourly), "now 1-d" for last 24h (every 8 minutes), "now 4-H" for last 4h (every 5 minutes), "now 4-H" for last 60min UTC (every minute), "Y-m-d Y-m-d" for time span between two dates)
#
source("extraction/extract_searchesGTrends.R")



# =========================================
# Extract leading indicators from OECD
# =========================================
# Parameters OECD (leading indicators):
selected_initial_year_OECD <- "2021"
selected_end_year_OECD <-as.character(year(Sys.Date()))
#
source("extraction/extract_indicatorsOECD.R")



# =========================================
# Extract Moon angle data
# =========================================
#
source("extraction/extract_moonData.R")



# =========================================
# Extract Twitter posts related data
# =========================================
#
source("extraction/extract_TWITTER.R")



# =========================================
# Extract from news
# =========================================
#
source("extraction/extract_newsData.R")














