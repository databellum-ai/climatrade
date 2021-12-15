## PTE: backup incorporado de los datos al principio de la extraccion

# =========================================
# DATA WE EXTRACT:
# ----------------
# Assets values, indexes, outcomes:
# !-STOCK&ASSETS PRICES [daily | . | ?? | YahooFinance] 
# -OECD CLI, BCI, CCI leading indicators [monthly | by_country | 1960 | OECD]
#
# Social symptoms/indicators:
# -Twitter POST SENTIMENTS value for given list of concepts [daily | global, by_concept | ?? | Twitter]
# !-SEARCHES RELATIVE-VOLUME OVER TIME [daily | global, by_concept | ?? | GoogleTrends]
# -AIR TRAFFIC [daily | by_city | 2016 | openSkies]
# -MUSIC STYLE of steams [daily | per_country | 2017 | Spotify]
# -FOOTBALL RANKING OF COUNTRIES [monthly | by_country&region | 1992 | FIFA]
#
# Earth influence on facts:
# -MOON PHASES, SUNRISE/SUNSET/NIGHTHOURS [daily | NYC | 1960 | suncalc]
# -DAILY WEATHER in key worldwide cities  [daily | by_city | 1989 | NOAA]
#
# Causality hypothesis ("seed")
# !-KAM (Key Asset to Model)
# !-KCH (KAM Causality Hypothesis)
#
#
#
# FUTURE DEVELOPMENTS:
# --------------------
# -GoogleTrends: analyze related search terms, not only interest-over-time
# -GoogleTrends: distinguish search results by country ("geo")
# -Twitter-fechas: valorar/PROBAR con Twitter problema de sólo 10 días hacia atrás...COMPROBAR NIVEL ACTUAL EN TWITTER ("Elevated?"): https://developer.twitter.com/en/docs/twitter-api/getting-started/about-twitter-api#Access
# -Separate Google searches per country
# -Separate Twitter posts per country
# -Twitter posts volume (currently using Google searchs for it)
# -Analysis ("tag cloud", etc.) of related term-searches to list given
# -News combined/coexisting terms (in same article) volume and sentiment
# -Use points instead of ranking in the FIFA classification


library(tidyverse)
library(lubridate)
library(ggthemes)


# ===============
# KEYS
# ---------------
source("keys_APIs.R")


# -----------------------------------------------------------------
# -----------------------------------------------------------------
# Extraction parameters ("seed")
# -----------------------------------------------------------------
# -----------------------------------------------------------------
#============ Relevant cities for weather and air traffic data
cities <- c("NewYork", "Paris", "HongKong", "London", "Beijing", "Madrid", "Tokyo")
addresses <- c("New York City, US", "Paris, France", "Hong Kong", "London, England", "Beijing, China", "Madrid, Spain", "Tokyo, Japan")
countries <- c("US", "FR", "CH", "UK", "CH", "SP", "JP")
airports <- c("KJFK", "LFPG", "VHHH", "EGLL", "ZBAA", "LEMD", "RJTT")
#============ Term for sentiment analysis:
sentimentTerms <- list("spanish_banks" = c("unicaja", "bbva", "banco santander"), 
                       "global_politics" = c("from:potus", "brexit", "from:borisjohnson"), 
                       "football" = c("real madrid", "psg", "messi"))
#============ Searches parameters (Google Trends):
searchTerms <- list("spanish_banks" = c("unicaja", "bbva", "banco santander"), 
                    "global_politics" = c("potus", "brexit", "boris johnson"), 
                    "football" = c("real madrid", "psg", "messi"),
                    "cinema" = c("veronica forque", "victoria abril"))
#============ Prices parameters (Yahoo! Finance):
chosenTickers = c("AAPL", "NFLX", "AMZN", "AAIC", "MSFT", "AAN")
endDateTicker = as.Date(today())
startingDateTicker = endDateTicker - 365 # also as... "2021-01-01"


# -----------------------------------------------------------------
# -----------------------------------------------------------------
# Data extraction from miscelaneous sources
# -----------------------------------------------------------------
# -----------------------------------------------------------------
source("extraction/extract_searchsGTrends.R")# Extract searches from Google Trends
source("extraction/extract_stocksPrices.R") # Extract stock prices from Yahoo Finance
source("extraction/extract_musicSPOTIFY.R") # Extract music trends from SPOTIFY
source("extraction/extract_rankingFIFA.R")# Extract from FIFA Ranking
source("extraction/extract_indicatorsOECD.R")# Extract leading indicators from OECD
source("extraction/extract_weatherNOAA.R")# Extract weather from NOAA
source("extraction/extract_airTraffic.R")# Extract air traffic data
source("extraction/extract_moonSunData.R")# Extract Moon and Sun related data (phase, night hours)
source("extraction/extract_sentimentsTwitter.R")# Extract Twitter posts sentiment data


# -----------------------------------------------------------------
# -----------------------------------------------------------------
# STANDARDIZE THE GEOGRAPHIC LOCATIONS FROM EACH EXTRACTED DATASET
# -----------------------------------------------------------------
# -----------------------------------------------------------------
# PTE: acabar left-join para crear campos de estándar (falta el stdCountryName)
# PTE: entrada "manual" de los códigos estándar
# PTE: guardar versión .XLS para "read-only"

geo_airTraffic <- readRDS("data/geo_airTraffic.rds")
geo_FIFA <- readRDS("data/geo_FIFA.rds")
geo_moonSun <- readRDS("data/geo_moonSun.rds")
geo_music <- readRDS("data/geo_music.rds")
geo_OECD <- readRDS("data/geo_OECD.rds")
geo_weather <- readRDS("data/geo_weather.rds")

head(geo_airTraffic)
head(geo_FIFA)
head(geo_moonSun)
head(geo_music)
head(geo_OECD)
head(geo_weather)

geo_airTraffic <- geo_airTraffic %>% 
  mutate(source="AirTraffic") %>% select(source, countryCode)
geo_FIFA <- geo_FIFA %>% 
  mutate(source="Football", CountryCode = toupper(CountryCode), countryName = CountryName, regionCode = Region) %>% 
  rename(countryCode = CountryCode) %>% 
  select(source, countryName, regionCode)
geo_moonSun <- geo_moonSun %>% 
  mutate(source="MoonSun", countryCode=toupper(countryCode)) %>% select(source, countryCode)
geo_music <- geo_music %>% 
  mutate(source="Music", countryCode=toupper(countryCode), countryName = country) %>% select(source, countryCode, countryName)
geo_OECD <- geo_OECD %>% 
  mutate(source="OECD", countryCode = toupper(LocationId), countryName = LocationName) %>% select(source, countryCode, countryName)
geo_weather <- geo_weather %>% 
  mutate(source="Weather", countryCode = toupper(countryId)) %>% select(source, countryCode)

# We bind all lists of codes
std_geo <- data.frame()
std_geo <- std_geo %>% 
  bind_rows(geo_airTraffic) %>% 
  bind_rows(geo_FIFA) %>% 
  bind_rows(geo_moonSun) %>% 
  bind_rows(geo_music) %>% 
  bind_rows(geo_OECD) %>% 
  bind_rows(geo_weather) %>% 
  arrange(countryCode, source)
view(std_geo)
head(std_geo)

tmp_std <- std_geo %>% filter(source=="Football") %>% select(stdCountryCode = countryCode, stdCountryName = countryName)
std_geo %>% left_join(tmp_std, by = c("countryCode" = "stdCountryName"))

# -----------------------------------------------------------------
# -----------------------------------------------------------------
# EDA
# -----------------------------------------------------------------
# -----------------------------------------------------------------
# Chart of interest over time for each search "concept"
all_searches %>% group_by(KAM) %>%
  ggplot(aes(x = date, y = normHits, color = time)) +
  geom_point(size = 1) +
  facet_wrap(~KAM) +
  ggthemes::theme_economist() + labs(title = "Interest over Time", subtitle = "Google Trends Report", caption = "By databellum®")

# Chart OECD
leadingIndicatorsOECD %>%
  ggplot(aes(as_date(Date), OECD_CLI, color=Country)) + geom_line()







