# -----------------------------------------------------------------
# -----------------------------------------------------------------
# Extract Features and Geography Catalogs
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
stocks <- readRDS("data/data_stocks_ts.rds")

head(airTraffic)
head(FIFA)
head(moonSun)
head(music)
head(OECD)
head(searchesGoogle)
head(twitterSentiment)
head(stocks)

airTraffic_features <- paste0("airTraffic.", airTraffic %>% select (-c(date)) %>% names())
FIFA_features <- paste0("FIFA.", FIFA %>% select (-c(Date, CountryCode)) %>% names())
moonSun_features <- paste0("moonSun.", moonSun %>% select (-c(date, countryCode)) %>% names())
music_features <- paste0("music.", music %>% select (-c(date, country, countryCode)) %>% names())
OECD_features <- paste0("OECD.", OECD %>% select (-c(Date, Country)) %>% names())
searchesGoogle_features <- paste0("searchesGoogle.", searchesGoogle %>% select (-c(date)) %>% names())
twitterSentiment_features <- paste0("twitterSentiment.", twitterSentiment %>% select (-c(date)) %>% names())
stocks_features <- paste0("stocks.", stocks %>% select (-c(date)) %>% names())

all_features <- c(
  airTraffic_features, 
  FIFA_features, 
  moonSun_features, 
  music_features, 
  OECD_features, 
  searchesGoogle_features, 
  twitterSentiment_features, 
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

all_features
head(std_geo_revised)
std_geo_list

# ------------------------------------------------------
# save for reference a spreadsheet containing catalog of available Features
# ------------------------------------------------------
write.xlsx(as_tibble(all_features), "userEdition/featuresCatalog_output.xlsx", overwrite = TRUE, colNames = FALSE)