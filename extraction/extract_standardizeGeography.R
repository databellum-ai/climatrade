# PTE: entrada "manual" de los códigos estándar

# -----------------------------------------------------------------
# -----------------------------------------------------------------
# STANDARDIZE THE GEOGRAPHIC LOCATIONS FROM EACH EXTRACTED DATASET
# -----------------------------------------------------------------
# -----------------------------------------------------------------

library(tidyverse)
library(openxlsx)

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

# we bind all lists of codes
all_geo <- data.frame()
all_geo <- all_geo %>% 
  bind_rows(geo_airTraffic) %>% 
  bind_rows(geo_FIFA) %>% 
  bind_rows(geo_moonSun) %>% 
  bind_rows(geo_music) %>% 
  bind_rows(geo_OECD) %>% 
  bind_rows(geo_weather) %>% 
  arrange(countryCode, source)
head(all_geo)

# we use as draft values the most complete list (FIFA football) 
tmp_std <- all_geo %>% 
  filter(source=="Football") %>% 
  mutate(stdCountryCode = countryCode, stdCountryName = countryName) %>% 
  group_by(countryCode) %>% summarise(stdCountryCode = first(stdCountryCode), stdCountryName = first(stdCountryName)) %>% 
  select(countryCode, stdCountryCode,stdCountryName)
nrow(tmp_std)
head(tmp_std)

# we add new draft standard columns 8stdCountryCode, stdCountryName) as a proposal fir further manual edition
std_geo <- all_geo %>% left_join(tmp_std, by = "countryCode")
nrow(std_geo)
head(std_geo)

# save a DRAFT (standardGeography_DRAFT.xlsx) so an administrator/user can use as reference to generate final file (standardGeography.xlsx)
write.xlsx(std_geo, "userEdition/standardGeography_DRAFT.xlsx", overwrite = TRUE)






