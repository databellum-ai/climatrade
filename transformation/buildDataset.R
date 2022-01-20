# PTE: Create network (https://www.jessesadler.com/post/network-analysis-with-r/)

# -----------------------------------------------------------------
# -----------------------------------------------------------------
# 
# -----------------------------------------------------------------
# -----------------------------------------------------------------

library(tidyverse)
library(openxlsx)


# ------------------------------------------------------
# Extract Features and Geography Catalogs
# ------------------------------------------------------

airTraffic <- readRDS("data/data_airTraffic_ts.rds")
FIFA <- readRDS("data/data_FIFA_ts.rds")
moonSun <- readRDS("data/data_moonSun_ts.rds")
music <- readRDS("data/data_music_ts.rds")
OECD <- readRDS("data/data_OECD_ts.rds")
searchesGoogle <- readRDS("data/data_searchesGoogle_ts.rds")
twitterSentiment <- readRDS("data/data_twitterSentiment_ts.rds")
weather <- readRDS("data/data_weather_ts.rds")
stocks <- readRDS("data/data_stocks_ts.rds")

head(airTraffic)
head(FIFA)
head(moonSun)
head(music)
head(OECD)
head(searchesGoogle)
head(twitterSentiment)
head(weather)
head(stocks)

airTraffic_features <- paste0("airTraffic.", airTraffic %>% select (-c(date, countryCode)) %>% names())
FIFA_features <- paste0("airTraffic.", FIFA %>% select (-c(Date)) %>% names())
moonSun_features <- paste0("moonSun.", moonSun %>% select (-c(date, countryCode)) %>% names())
music_features <- paste0("music.", music %>% select (-c(date, country, countryCode)) %>% names())
OECD_features <- paste0("OECD.", OECD %>% select (-c(Date, Country)) %>% names())
searchesGoogle_features <- paste0("searchesGoogle.", searchesGoogle %>% select (-c(date)) %>% names())
twitterSentiment_features <- paste0("twitterSentiment.", twitterSentiment %>% select (-c(date)) %>% names())
weather_features <- paste0("weather.", weather %>% select (-c(date, stationPlace)) %>% names())
stocks_features <- paste0("stocks.", stocksData %>% select (-c(date)) %>% names())

all_features <- c(
  airTraffic_features, 
  FIFA_features, 
  moonSun_features, 
  music_features, 
  OECD_features, 
  searchesGoogle_features, 
  twitterSentiment_features, 
  weather_features, 
  stocks_features)

all_features



# ------------------------------------------------------
# Extract Features and Geography Catalogs
# ------------------------------------------------------

std_geo_revised <- read.xlsx("userEdition/standardGeography.xlsx")
std_geo_list <- unique(std_geo_revised$stdCountryName)

# ------------------------------------------------------
# Print obtained catalogs
# ------------------------------------------------------

print("AVAILABLE FEATURES:")
print(all_features)
print("AVAILABLE GEOGRAPHY DIMENSIONS:")
head(std_geo_revised)
print(std_geo_list)

