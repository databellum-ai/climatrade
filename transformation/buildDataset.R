# Comprobar en profundidad datos consolidados (merge) de: fullDataset_raw


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
# Convert locations align field names, add geo field if not present
# ------------------------------------------------------

# Get standard geo codes and align field names
reduced_std_geo <- std_geo %>% select(source, countryCode, stdCountryCode)

music <- music %>% 
  mutate(countryCode = toupper(countryCode)) %>% 
  left_join((reduced_std_geo %>% filter(source=="music")), by = "countryCode") %>% 
  select(-c("countryCode", "country"))

FIFA <- FIFA %>% 
  mutate(CountryCode = toupper(CountryCode)) %>% 
  left_join((reduced_std_geo %>% filter(source=="FIFA")), by = c("CountryCode" = "countryCode")) %>% 
  rename("date" = "Date") %>% 
  select(-c("CountryCode", "source"))
  
moonSun <- moonSun %>% 
  mutate(countryCode = toupper(countryCode)) %>% 
  left_join((reduced_std_geo %>% filter(source=="moonSun")), by = "countryCode") %>% 
  select(-c("countryCode", "source"))
moonSun %>% filter(date =="1960-01-01")

OECD <- OECD %>% 
  mutate(countryCode = toupper(Country)) %>% 
  left_join((reduced_std_geo %>% filter(source=="OECD")), by = c("Country" = "countryCode")) %>% 
  rename("date" = "Date") %>% 
  select(-c("countryCode", "Country", "source"))


# Add geo field (as NA) if not present, to facilitate merge
airTraffic <- airTraffic %>% mutate(stdCountryCode = NA)
searchesGoogle <- searchesGoogle %>% mutate(stdCountryCode = NA)
twitterSentiment <- twitterSentiment %>% mutate(stdCountryCode = NA)
stocks <- stocks %>% mutate(stdCountryCode = NA)



# ------------------------------------------------------
# Merge a dataset based specifically in the seed (hypothesis)
# ------------------------------------------------------



# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# test1 <- data.frame(
#   date=c("2021/12/20", "2021/12/20", "2021/12/19", "2021/12/19", "2021/12/18", "2021/12/18", "2021/12/17", "2021/12/17"),
#   vbleJ=c(2300, 4800, 5500, 8300, 3900, 700, 900, 1300), 
#   vbleK=c(12300, 14800, 15500, 18300, 13900, 1700, 9100, 12800), 
#   vbleL=c(3, 3, 3, 5, 6, 8, 5, 7),
#   stdCountryCode=c("ESP", "US", "ESP", "US", "FR", "US", "ESP", "FR"))
# test2 <- data.frame(
#   date=c("2021/12/20", "2021/12/20", "2021/12/19", "2021/12/19", "2021/12/18", "2021/12/18"),
#   vbleA=c(23, 48, 55, 83, 39, 7), 
#   vbleB=c(123, 148, 155, 183, 139, 17),
#   stdCountryCode=c("ESP", NA, NA, NA, NA, "FR"))
# test1
# test2
# 
# # Outer Join: 
# merge(test1, test2, by = c("date", "stdCountryCode"), all=TRUE)
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




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


# ============================================================================
# ============================================================================
# ============================================================================
# ============================================================================


# ------------------------------------------------------
# Load consolidated raw dataset, still to refine
# ------------------------------------------------------
fullDataset_raw <- readRDS("data/dataset_raw.rds")
head(fullDataset_raw)


fullDataset_raw %>% filter(is.na(stdCountryCode)) %>% arrange(desc(date))
fullDataset_raw %>% filter(!is.na(stdCountryCode)) %>% arrange(desc(date))


# ------------------------------------------------------
# Remove not applying features and keep only defined geo-filters
# ------------------------------------------------------


