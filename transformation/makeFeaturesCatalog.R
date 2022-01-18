
# -----------------------------------------------------------------
# -----------------------------------------------------------------
# READ EXTRACTED DATA
# -----------------------------------------------------------------
# -----------------------------------------------------------------
# 

library(tidyverse)

data_airTraffic_ts <- readRDS("data/data_airTraffic_ts.rds")
data_FIFA_ts <- readRDS("data/data_FIFA_ts.rds")
data_moonSun_ts <- readRDS("data/data_moonSun_ts.rds")
data_music_ts <- readRDS("data/data_music_ts.rds")
data_OECD_ts <- readRDS("data/data_OECD_ts.rds")
data_searchesGoogle_ts <- readRDS("data/data_searchesGoogle_ts.rds")
data_twitterSentiment_ts <- readRDS("data/data_twitterSentiment_ts.rds")
data_weather_ts <- readRDS("data/data_weather_ts.rds")
data_stocksData_ts <- readRDS("data/data_stocksData_ts.rds")

head(data_airTraffic_ts)
head(data_FIFA_ts)
head(data_moonSun_ts)
head(data_music_ts)
head(data_OECD_ts)
head(data_searchesGoogle_ts)
head(data_twitterSentiment_ts)
head(data_weather_ts)
head(data_stocksData_ts)

airTraffic_features <- data_airTraffic_ts %>% select (-c(date, countryCode)) %>% names()
FIFA_features <- data_FIFA_ts %>% select (-c(Date)) %>% names()
moonSun_features <- data_moonSun_ts %>% select (-c(date, countryCode)) %>% names()
music_features <- data_music_ts %>% select (-c(date, country, countryCode)) %>% names()
OECD_features <- data_OECD_ts %>% select (-c(Date, Country)) %>% names()
searchesGoogle_features <- data_searchesGoogle_ts %>% select (-c(date)) %>% names()
twitterSentiment_features <- data_twitterSentiment_ts %>% select (-c(date)) %>% names()
weather_features <- data_weather_ts %>% select (-c(date, stationPlace)) %>% names()
stocksData_features <- data_stocksData_ts %>% select (-c(date)) %>% names()

all_features <- c(airTraffic_features, 
  FIFA_features, 
  moonSun_features, 
  music_features, 
  OECD_features, 
  searchesGoogle_features, 
  twitterSentiment_features, 
  weather_features, 
  stocksData_features)

all_features

