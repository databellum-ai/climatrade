
# -----------------------------------------------------------------
# -----------------------------------------------------------------
# STANDARDIZE THE GEOGRAPHIC LOCATIONS FROM EACH EXTRACTED DATASET
# -----------------------------------------------------------------
# -----------------------------------------------------------------
# 
# This code is used to standardize the country/geography codes coming from each source
# Approach is to generate a DRAT file contiing all codes merged in a singles list specifying their origin. This list includes also two new columns (standard code and country name) that will contain the standardized values:
#  Step 1: we generate a draft list in an Excel files called "/userEdition/standardGeography_DRAFT.xlsx"
#  Step 2: an authorized user fill standard unified values for new columns and saves populated list as file as "/userEdition/standardGeography_DRAFT.xlsx"
#  Step 3: list contained in the new file will solve the correspondence between country codes in each original dataset and standard codes
#  
# NOTE: we use an extended meaning of country that combines countrys, nations or even groups of countries. Eg: for FIFA data we get England or Wales, for OECD data we get countries or groups like NAFTA or G7, even for FIFA data, we have a different column for group (UEFA, CONCACAF)
# NOTE: regions/groups of countries are ignored now. In other pieces of code, regions could be used as aggregators, wether for regions already contained in the initial file (eg FIFA), or by using other external table to aggregate single records in current list


library(tidyverse)
library(openxlsx)

geo_FIFA <- readRDS("data/geo_FIFA.rds")
geo_moonSun <- readRDS("data/geo_moonSun.rds")
geo_music <- readRDS("data/geo_music.rds")
geo_OECD <- readRDS("data/geo_OECD.rds")

head(geo_FIFA)
head(geo_moonSun)
head(geo_music)
head(geo_OECD)

geo_FIFA <- geo_FIFA %>% 
  mutate(source="FIFA", CountryCode = toupper(CountryCode), countryName = CountryName, regionCode = Region) %>% 
  rename(countryCode = CountryCode) %>% 
  select(source, countryCode, countryName, regionCode)
geo_moonSun <- geo_moonSun %>% 
  mutate(source="moonSun", countryCode=toupper(countryCode)) %>% select(source, countryCode)
geo_music <- geo_music %>% 
  mutate(source="music", countryCode=toupper(countryCode), countryName = country) %>% select(source, countryCode, countryName)
geo_OECD <- geo_OECD %>% 
  mutate(source="OECD", countryCode = toupper(LocationId), countryName = LocationName) %>% select(source, countryCode, countryName)

# we bind all lists of codes
all_geo <- data.frame()
all_geo <- all_geo %>% 
  bind_rows(geo_FIFA) %>% 
  bind_rows(geo_moonSun) %>% 
  bind_rows(geo_music) %>% 
  bind_rows(geo_OECD) %>% 
  arrange(countryCode, source)
head(all_geo)

# we use as draft values the most complete list (FIFA football) 
tmp_std <- all_geo %>% 
  filter(source=="FIFA") %>% 
  mutate(stdCountryCode = countryCode, stdCountryName = countryName) %>% 
  group_by(countryCode) %>% summarise(stdCountryCode = first(stdCountryCode), stdCountryName = first(stdCountryName)) %>% 
  select(countryCode, stdCountryCode,stdCountryName)
nrow(tmp_std)
head(tmp_std)

# we add new draft standard columns (stdCountryCode, stdCountryName) as a proposal for further manual edition
std_geo <- all_geo %>% left_join(tmp_std, by = "countryCode")
nrow(std_geo)
head(std_geo)

# save a DRAFT (standardGeography_DRAFT.xlsx) so an administrator/user can use as reference to generate final file (standardGeography.xlsx)
write.xlsx(std_geo, "userEdition/standardGeography_DRAFT.xlsx", overwrite = TRUE)






