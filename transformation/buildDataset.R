library(tidyverse)

airTraffic <- readRDS("data/data_airTraffic_ts.rds")
FIFA <- readRDS("data/data_FIFA_ts.rds")
moonSun <- readRDS("data/data_moonSun_ts.rds")
music <- readRDS("data/data_music_ts.rds")
OECD <- readRDS("data/data_OECD_ts.rds")
searchesGoogle <- readRDS("data/data_searchesGoogle_ts.rds")
twitterSentiment <- readRDS("data/data_twitterSentiment_ts.rds")
weather <- readRDS("data/data_weather_ts.rds")
stocksData <- readRDS("data/data_stocksData_ts.rds")

head(airTraffic)
head(FIFA)
head(moonSun)
head(music)
head(OECD)
head(searchesGoogle)
head(twitterSentiment)
head(weather)
head(stocksData)

airTraffic_features <- airTraffic %>% select (-c(date, countryCode)) %>% names() %>% cbind(source="airTraffic") %>% as_tibble()
FIFA_features <- FIFA %>% select (-c(Date)) %>% names() %>% cbind(source="FIFA") %>% as_tibble()
moonSun_features <- moonSun %>% select (-c(date, countryCode)) %>% names() %>% cbind(source="moonSun") %>% as_tibble()
music_features <- music %>% select (-c(date, country, countryCode)) %>% names() %>% cbind(source="music") %>% as_tibble()
OECD_features <- OECD %>% select (-c(Date, Country)) %>% names() %>% cbind(source="OECD") %>% as_tibble()
searchesGoogle_features <- searchesGoogle %>% select (-c(date)) %>% names() %>% cbind(source="searchesGoogle") %>% as_tibble()
twitterSentiment_features <- twitterSentiment %>% select (-c(date)) %>% names() %>% cbind(source="twitterSentiment") %>% as_tibble()
weather_features <- weather %>% select (-c(date, stationPlace)) %>% names() %>% cbind(source="weather") %>% as_tibble()
stocksData_features <- stocksData %>% select (-c(date)) %>% names() %>% cbind(source="stocksDat") %>% as_tibble()

all_features <- rbind(airTraffic_features, 
                      FIFA_features, 
                      moonSun_features, 
                      music_features, 
                      OECD_features, 
                      searchesGoogle_features, 
                      twitterSentiment_features, 
                      weather_features, 
                      stocksData_features)
names(all_features)[1] <- "feature"

head(all_features)
