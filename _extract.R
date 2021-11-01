# -----------------------------------------------------------------
# -----------------------------------------------------------------
# Data extraction from miscelaneous sources
# -----------------------------------------------------------------
# -----------------------------------------------------------------

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
relevantCities <- data.frame(
  id = c("NewYork", "Oviedo", "Paris", "HongKong", "London", "Beijng", "Madrid", "Albacete", "Tokyo"),
  latitude = c(40.70623940806975, 43.3620683921967, 48.8613182352403, 22.32029644568666, 51.50702741724013, 39.905384001792335, 40.425619645599916, 39.267266932791685, 35.687667406759765),
  longitude = c(-74.00883633105707, -5.84817121485434, 2.3003412809927495, 114.19091287611904, -0.12701173276875632, 116.37699181234836, -3.7025627487487984, -1.5500112927257998, 139.76554769212072))
#
source("extarct_weatherNOAA.R")
allStationsData <- readRDS("data/data_weather_ts.rds") # time-series format
# allStationsData <- readRDS("data/data_weather_sp.rds") # spread format




# =======================================
# Save to RDS
# =======================================
# allData <- rbind(..., ..., ...)
# head(allData)
# saveRDS(allData, "data/data_extracted.rds")
# write.csv(allData, "data/data_extracted.csv")



