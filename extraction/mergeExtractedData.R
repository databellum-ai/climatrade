library(tidyverse)
library(openxlsx)
library(lubridate)  # To create absolute list of dates

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
# Convert locations align field names, add geo field if not present
# ------------------------------------------------------

# ------------------------------------------------------
# Load standard geography (previously validated by user)
std_geo <- read.xlsx("userEdition/standardGeography.xlsx")
head(std_geo)
# Get standard geo codes and align field names
reduced_std_geo <- std_geo %>% select(source, countryCode, stdCountryCode)
head(reduced_std_geo)

music <- music %>% 
  mutate(countryCode = toupper(countryCode)) %>% 
  left_join((reduced_std_geo %>% filter(source=="music")), by = "countryCode") %>% 
  select(-c("countryCode", "country"))
names(music) <- paste0("music.", names(music))
music <- music %>% rename(date = music.date)
music <- music %>% rename(stdCountryCode = music.stdCountryCode)

FIFA <- FIFA %>% 
  mutate(CountryCode = toupper(CountryCode)) %>% 
  left_join((reduced_std_geo %>% filter(source=="FIFA")), by = c("CountryCode" = "countryCode")) %>% 
  rename("date" = "Date") %>% 
  select(-c("CountryCode", "source"))
names(FIFA) <- paste0("FIFA.", names(FIFA))
FIFA <- FIFA %>% rename(date = FIFA.date)
FIFA <- FIFA %>% rename(stdCountryCode = FIFA.stdCountryCode)

moonSun <- moonSun %>% 
  mutate(countryCode = toupper(countryCode)) %>% 
  left_join((reduced_std_geo %>% filter(source=="moonSun")), by = "countryCode") %>% 
  select(-c("countryCode", "source"))
moonSun %>% filter(date =="1960-01-01")
names(moonSun) <- paste0("moonSun.", names(moonSun))
moonSun <- moonSun %>% rename(date = moonSun.date)
moonSun <- moonSun %>% rename(stdCountryCode = moonSun.stdCountryCode)

OECD <- OECD %>% 
  mutate(countryCode = toupper(Country)) %>% 
  left_join((reduced_std_geo %>% filter(source=="OECD")), by = c("Country" = "countryCode")) %>% 
  rename("date" = "Date") %>% 
  select(-c("countryCode", "Country", "source"))
names(OECD) <- paste0("OECD.", names(OECD))
OECD <- OECD %>% rename(date = OECD.date)
OECD <- OECD %>% rename(stdCountryCode = OECD.stdCountryCode)

# Add geo field (as "GLOBAL") if not present, to facilitate merge
airTraffic <- airTraffic %>% mutate(stdCountryCode = "GLOBAL")
names(airTraffic) <- paste0("airTraffic.", names(airTraffic))
airTraffic <- airTraffic %>% rename(date = airTraffic.date)
airTraffic <- airTraffic %>% rename(stdCountryCode = airTraffic.stdCountryCode)

searchesGoogle <- searchesGoogle %>% mutate(stdCountryCode = "GLOBAL")
names(searchesGoogle) <- paste0("searchesGoogle.", names(searchesGoogle))
searchesGoogle <- searchesGoogle %>% rename(date = searchesGoogle.date)
searchesGoogle <- searchesGoogle %>% rename(stdCountryCode = searchesGoogle.stdCountryCode)

twitterSentiment <- twitterSentiment %>% mutate(stdCountryCode = "GLOBAL")
names(twitterSentiment) <- paste0("twitterSentiment.", names(twitterSentiment))
twitterSentiment <- twitterSentiment %>% rename(date = twitterSentiment.date)
twitterSentiment <- twitterSentiment %>% rename(stdCountryCode = twitterSentiment.stdCountryCode)

stocks <- stocks %>% mutate(stdCountryCode = "GLOBAL")
names(stocks) <- paste0("stocks.", names(stocks))
stocks <- stocks %>% rename(date = stocks.date)
stocks <- stocks %>% rename(stdCountryCode = stocks.stdCountryCode)

# ------------------------------------------------------
# Merge a dataset based specifically in the seed (hypothesis)
# ------------------------------------------------------

head(airTraffic)
head(FIFA)
head(moonSun)
head(music)
head(OECD)
head(searchesGoogle)
head(twitterSentiment)
head(stocks)

fullDataset_raw <- data.frame()
fullDataset_raw <- merge(airTraffic, FIFA, by = c("date", "stdCountryCode"), all=TRUE)
fullDataset_raw <- merge(fullDataset_raw, moonSun, by = c("date", "stdCountryCode"), all=TRUE)
fullDataset_raw <- merge(fullDataset_raw, music, by = c("date", "stdCountryCode"), all=TRUE)
fullDataset_raw <- merge(fullDataset_raw, OECD, by = c("date", "stdCountryCode"), all=TRUE)
fullDataset_raw <- merge(fullDataset_raw, searchesGoogle, by = c("date", "stdCountryCode"), all=TRUE)
fullDataset_raw <- merge(fullDataset_raw, twitterSentiment, by = c("date", "stdCountryCode"), all=TRUE)
fullDataset_raw <- merge(fullDataset_raw, stocks, by = c("date", "stdCountryCode"), all=TRUE)

# ------------------------------------------------------
# Save consolidated raw dataset, still to refine
# ------------------------------------------------------
saveRDS(fullDataset_raw,"data/dataset_raw.rds")

