
library(tidyverse)
library(openxlsx)

# ------------------------------------------------------
# Extract seed (features and locations to use as hypothesis)
# ------------------------------------------------------
allFeatures_df <- readRDS("data/featuresSeed.rds")
head(allFeatures_df)
# ------------------------------------------------------
# Load standard geography (previously validated by user)
# ------------------------------------------------------
std_geo <- read.xlsx("userEdition/standardGeography.xlsx")
head(std_geo)


# ------------------------------------------------------
# Get data from extracted .RDS files
# ------------------------------------------------------
airTraffic <- readRDS("data/data_airTraffic_ts.rds")
FIFA <- readRDS("data/data_FIFA_ts.rds")
moonSun <- readRDS("data/data_moonSun_ts.rds")
music <- readRDS("data/data_music_ts.rds")
OECD <- readRDS("data/data_OECD_ts.rds")
searchesGoogle <- readRDS("data/data_searchesGoogle_ts.rds")
twitterSentiment <- readRDS("data/data_twitterSentiment_ts.rds")
stocks <- readRDS("data/data_stocks_ts.rds")


# ------------------------------------------------------
# Convert locations to standard and field names
# ------------------------------------------------------
reduced_std_geo <- std_geo %>% select(source, countryCode, stdCountryCode)

# Create standard geolocations, align field names
music <- music %>% 
  mutate(countryCode = toupper(countryCode)) %>% 
  left_join((reduced_std_geo %>% filter(source=="music")), by = "countryCode") %>% 
  select(-c("countryCode", "country"))

# Create standard geolocations, align field names
FIFA <- FIFA %>% 
  mutate(CountryCode = toupper(CountryCode)) %>% 
  left_join((reduced_std_geo %>% filter(source=="FIFA")), by = c("CountryCode" = "countryCode")) %>% 
  rename("date" = "Date") %>% 
  select(-c("CountryCode", "source"))
  
# Create standard geolocations, align field names
moonSun <- moonSun %>% 
  mutate(countryCode = toupper(countryCode)) %>% 
  left_join((reduced_std_geo %>% filter(source=="moonSun")), by = "countryCode") %>% 
  select(-c("countryCode", "source"))
moonSun %>% filter(date =="1960-01-01")

# Create standard geolocations, align field names
OECD <- OECD %>% 
  mutate(countryCode = toupper(Country)) %>% 
  left_join((reduced_std_geo %>% filter(source=="OECD")), by = c("Country" = "countryCode")) %>% 
  rename("date" = "Date") %>% 
  select(-c("countryCode", "Country", "source"))



head(airTraffic)
head(FIFA)
head(moonSun)
head(music)
head(OECD)
head(searchesGoogle)
head(twitterSentiment)
head(stocks)



head(twitterSentiment)
head(OECD)


# ------------------------------------------------------
# Merge a dataset based specifically in the seed (hypothesis)
# ------------------------------------------------------




