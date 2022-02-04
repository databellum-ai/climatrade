# Comprobar en profundidad datos consolidados (merge) de: fullDataset_raw


library(tidyverse)
library(openxlsx)
library(data.table)  # For dcast() to spread multiple columns

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
# Extract seed (features and locations to use as hypothesis)
allFeatures_df <- readRDS("data/featuresSeed.rds")
head(allFeatures_df)
# Extract header names of the seed features
seedVbles <- allFeatures_df %>% 
  filter(source != "locations") %>% 
  mutate(vbleName = paste0(source, ".", variable)) %>% 
  pull(vbleName)
seedVbles

# Get standard geo codes and align field names
reduced_std_geo <- std_geo %>% select(source, countryCode, stdCountryCode)

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

# Add geo field (as NA) if not present, to facilitate merge
airTraffic <- airTraffic %>% mutate(stdCountryCode = NA)
names(airTraffic) <- paste0("airTraffic.", names(airTraffic))
airTraffic <- airTraffic %>% rename(date = airTraffic.date)
airTraffic <- airTraffic %>% rename(stdCountryCode = airTraffic.stdCountryCode)

searchesGoogle <- searchesGoogle %>% mutate(stdCountryCode = NA)
names(searchesGoogle) <- paste0("searchesGoogle.", names(searchesGoogle))
searchesGoogle <- searchesGoogle %>% rename(date = searchesGoogle.date)
searchesGoogle <- searchesGoogle %>% rename(stdCountryCode = searchesGoogle.stdCountryCode)

twitterSentiment <- twitterSentiment %>% mutate(stdCountryCode = NA)
names(twitterSentiment) <- paste0("twitterSentiment.", names(twitterSentiment))
twitterSentiment <- twitterSentiment %>% rename(date = twitterSentiment.date)
twitterSentiment <- twitterSentiment %>% rename(stdCountryCode = twitterSentiment.stdCountryCode)

stocks <- stocks %>% mutate(stdCountryCode = NA)
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
head(twitterSentiment)
head(OECD)

fullDataset_raw <- merge(airTraffic, FIFA, by = c("date", "stdCountryCode"), all=TRUE)
fullDataset_raw <- merge(fullDataset_raw, moonSun, by = c("date", "stdCountryCode"), all=TRUE)
fullDataset_raw <- merge(fullDataset_raw, music, by = c("date", "stdCountryCode"), all=TRUE)
fullDataset_raw <- merge(fullDataset_raw, OECD, by = c("date", "stdCountryCode"), all=TRUE)
fullDataset_raw <- merge(fullDataset_raw, searchesGoogle, by = c("date", "stdCountryCode"), all=TRUE)
fullDataset_raw <- merge(fullDataset_raw, twitterSentiment, by = c("date", "stdCountryCode"), all=TRUE)
fullDataset_raw <- merge(fullDataset_raw, stocks, by = c("date", "stdCountryCode"), all=TRUE)


# convert country names from seed in standard codes
geoCodesSeed <- allFeatures_df %>% 
  filter(source=="locations") %>% select(variable) %>% 
  left_join(std_geo, by = c("variable" = "countryName")) %>% pull(stdCountryCode)
# filter only geolocations specidfied in the seed
fullDataset_raw <- fullDataset_raw %>% filter(stdCountryCode %in% c(NA, geoCodesSeed))


# ------------------------------------------------------
# Save consolidated raw dataset, still to refine
# ------------------------------------------------------
saveRDS(fullDataset_raw,"data/dataset_raw.rds")


# ============================================================================
# ============================================================================
# ============================================================================
# ============================================================================


# ------------------------------------------------------
# Remove not applying features 
# ------------------------------------------------------
# Load consolidated raw dataset, still to refine
fullDataset_raw <- readRDS("data/dataset_raw.rds")

# Reduce features to those in seed and also in actually obtained data
seedVbles <- seedVbles[seedVbles %in% names(fullDataset_raw)]
# Keep remaining columns not forgetting to include date and geo dimensions
seedDataset <- fullDataset_raw %>% select(date, stdCountryCode, seedVbles)




# Spread geo locations for each one of the features

# https://stackoverflow.com/questions/26019915/how-to-spread-or-cast-multiple-values-in-r
data <- data.frame(x=rep(c("red","blue","green"),each=4), y=rep(letters[1:4],3), value.1 = 1:12, value.2 = 13:24)
data %>%
  gather(Var, val, starts_with("value")) %>% 
  unite(Var1,Var, y) %>% 
  spread(Var1, val)

dcast(setDT(data), x~y, value.var=c('value.1', 'value.2'))

library(reshape2)
data1 <- melt(data, id.vars = c("x", "y"))
dcast(data1, x ~ variable + y)



dcast(setDT(seedDataset), date~stdCountryCode, value.var=seedVbles)

data2 <- melt(seedDataset, id.vars = c("date", "stdCountryCode"))
dcast(data2, date ~ variable + stdCountryCode)





# ------------------------------------------------------
# Save transformed dataset
# ------------------------------------------------------
saveRDS(seedDataset,"data/dataset_seed.rds")



