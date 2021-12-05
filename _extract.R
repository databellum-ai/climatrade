## PTE: backup incorporado de los datos al principio de la extraccion

# =========================================
# DATA WE EXTRACT:
# ----------------
#   * Assets values, indexes:
# -STOCK&ASSETS PRICES [daily | . | ?? | YahooFinance] 
# --OECD CLI, BCI, CCI leading indicators [monthly | by_country | 1960 | OECD]
#   * Social symptoms/indicators:
# -SEARCHES RELATIVE VOLUME OVER TIME [daily | global, by_concept | ?? | GoogleTrends]
# -Twitter POST SENTIMENTS value for given list of concepts [daily | global, by_concept | ?? | Twitter]
# --MUSIC STYLE of steams [daily | per_country | 2017 | Spotify]
# --FOOTBALL RANKING OF COUNTRIES [monthly | by_country&region | 1992 | FIFA]
# -AIR TRAFFIC [daily | by_city | 2012 | openSkies]
#   * Earth influence on facts:
# -MOON PHASES, TIDES table in NYC, SUNRISE/SUNSET/NIGHTHOURS [daily | NYC | 1960 | suncalc, rtide]
# --DAILY WEATHER in key worldwide cities  [daily | by_city | 1989 | NOAA]
#   * Causality hypothesis ("seed")
# -KAM (Key Asset to Model)
# -KCH (KAM Causality Hypothesis)
#
#
# FUTURE DEVELOPMENTS:
# --------------------
# -Separate Google searches per country
# -Separate Twitter posts per country
# -Twitter posts volume (currently using Google searchs for it)
# -Analysis ("tag cloud", etc.) of related term-searches to list given
# -News combined/coexisting terms (in same article) volume and sentiment
# -Use points instead of ranking in the FIFA classification



# =========================================

# Declare our relevant cities
cities <- c("NewYork", "Paris", "HongKong", "London", "Beijing", "Madrid", "Tokyo")
addresses <- c("New York City, US", "Paris, France", "Hong Kong", "London, England", "Beijing, China", "Madrid, Spain", "Tokyo, Japan")
countries <- c("US", "FR", "CH", "UK", "CH", "SP", "JP")
airports <- c("KJFK", "LFPG", "VHHH", "EGLL", "ZBAA", "LEMD", "RJTT")

# -----------------------------------------------------------------
# -----------------------------------------------------------------
# Data extraction from miscelaneous sources
# -----------------------------------------------------------------
# -----------------------------------------------------------------


# =========================================
# Extract music trends from SPOTIFY
# =========================================
source("extraction/extract_musicSPOTIFY.R") 


# =========================================
# Extract from FIFA Ranking
# =========================================
source("extraction/extract_rankingFIFA.R")


# =========================================
# Extract leading indicators from OECD
# =========================================
source("extraction/extract_indicatorsOECD.R")
# Chart
leadingIndicatorsOECD %>%
  ggplot(aes(as_date(Date), OECD_CLI, color=Country)) + geom_line()



# =========================================
# Extract weather from NOAA
# =========================================
source("extraction/extract_weatherNOAA.R")


# =========================================
# Extract air traffic data data
# =========================================
source("extraction/extract_airTraffic.R")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# =========================================
# Extract calendar data (Moon angle + Weekday) data
# =========================================
source("extraction/extract_moonData.R")


# =========================================
# Extract stock prices from Yahoo Finance
# =========================================
# Prices parameters:
chosenTickers = c("AAPL", "NFLX", "AMZN", "AAIC", "MSFT", "AAN")
endDateTicker = as.Date(today())
startingDateTicker = endDateTicker - 365 # also as... "2021-01-01"
source("extraction/extract_stocksPrices.R")


# =========================================
# Extract searches from Google Trends
# =========================================
# Searches parameters:
search_concept_gral <- "Banks"
search_concepts <- c("unicaja", "bbva", "santander")
search_places = c("ES", "ES", "ES") # ("" for all)
search_period <- "today 12-m" # Examples: "all" for all (since 1jan2004 monthly), "today+5-y" for last five years (default, weekly), "today 12-m" for 12 month from today (weekly), "today 3-m" for 3 months from today (daily), "now 7-d" for last week (hourly), "now 1-d" for last 24h (every 8 minutes), "now 4-H" for last 4h (every 5 minutes), "now 4-H" for last 60min UTC (every minute), "Y-m-d Y-m-d" for time span between two dates)
source("extraction/extract_searchsGTrends.R")


# =========================================
# Extract Twitter posts sentiment data
# =========================================
source("extraction/extract_TWITTER.R")





