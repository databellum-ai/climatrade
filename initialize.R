# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# INITIALIZATION
# -Environment initialization. Load all packages required
# -Data scope to retrieve 
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

# ===============
# CLEAN ENVIRONMENT
# ---------------
# rm(list = ls())

# ===============
# PACKAGES REQUIRED
# ---------------
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(gtrendsR)) install.packages("gtrendsR", repos = "http://cran.us.r-project.org") # To get GoogleTrends data
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org") # To improve charts
if(!require(jsonlite)) install.packages("jsonlite", repos = "http://cran.us.r-project.org")
if(!require(spotifyr)) install.packages("spotifyr", repos = "http://cran.us.r-project.org")
if(!require(rnoaa)) install.packages("rnoaa", repos = "http://cran.us.r-project.org")
if(!require(OECD)) install.packages("OECD", repos = "http://cran.us.r-project.org")
if(!require(gtools)) install.packages("gtools", repos = "http://cran.us.r-project.org")
if(!require(tidygeocoder)) install.packages("tidygeocoder", repos = "http://cran.us.r-project.org") # Resolve coordinates of cities/places
if(!require(openSkies)) install.packages("openSkies", repos = "http://cran.us.r-project.org") # Air traffic data from OpenSkies
if(!require(suncalc)) install.packages("suncalc", repos = "http://cran.us.r-project.org") # Moon and Sun daily data
if(!require(ROAuth)) install.packages("ROAuth", repos = "http://cran.us.r-project.org") # Moon and Sun daily data
if(!require(twitteR)) install.packages("twitteR", repos = "http://cran.us.r-project.org") # Moon and Sun daily data
if(!require(syuzhet)) install.packages("syuzhet", repos = "http://cran.us.r-project.org") # sentiment analysis
if(!require(openxlsx)) install.packages("openxlsx", repos = "http://cran.us.r-project.org") # import/export Excel
if(!require(tidyquant)) install.packages("tidyquant", repos = "http://cran.us.r-project.org") # To get stock data
if(!require(imputeTS)) install.packages("imputeTS", repos = "http://cran.us.r-project.org") # To impute values in time series
if(!require(bigrquery)) install.packages("bigrquery", repos = "http://cran.us.r-project.org") # To extract from GDELT using BigQuery
# INSTALL SCRAPE PACKAGES:
# + Ensure java installed
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")  # to scrape static pages
if(!require(RSelenium)) install.packages("RSelenium", repos = "http://cran.us.r-project.org")  # to scrape dynamic  pages, includes binman
if(!require(wdman)) install.packages("wdman", repos = "http://cran.us.r-project.org")  # for chromedriver install
# + Install chromedriver with function chrome() of package "wdman":
# cDrv <- chrome()

# ===============
# ENVIRONMENT CONSTANTS
# ---------------
absoluteInitialDate <- "1960-01-01"

# ===============
# GLOBAL FUNCTIONS
# ---------------

library(tidyverse)
library(openxlsx)

# Function to load available active sources to be used during extraction and transformation phases
sourcesAvailable <- function() {
  extractedEntities <- data.frame()
  extractedEntities <- rbind(extractedEntities, c("airTraffic", "data/data_airTraffic_ts.rds", "extraction/extract_airTraffic.R", "Air traffic data"))
  extractedEntities <- rbind(extractedEntities, c("OECD", "data/data_OECD_ts.rds", "extraction/extract_indicatorsOECD.R", "Leading indicators from OECD"))
  extractedEntities <- rbind(extractedEntities, c("stocks", "data/data_stocks_ts.rds", "extraction/extract_stocksPrices.R", "Stock prices from Yahoo Finance"))
  extractedEntities <- rbind(extractedEntities, c("searchesGoogle", "data/data_searchesGoogle_ts.rds", "extraction/extract_searchsGTrends.R", "Searches from Google Trends"))
  # extractedEntities <- rbind(extractedEntities, c("GDELT", "data/data_GDELT_ts.rds", "extraction/extract_GDELT.R", "Worls events tone and impact"))
  extractedEntities <- rbind(extractedEntities, c("index", "data/data_index_ts.rds", "extraction/extract_index.R", "Indexes from disparate origins"))
  extractedEntities <- rbind(extractedEntities, c("music", "data/data_music_ts.rds", "extraction/extract_musicSPOTIFY.R", "Music trends from SPOTIFY"))  
  names(extractedEntities) <- c("Preffix", "DataFileName", "SourceCode", "Description")
  extractedEntities
}

# -----------------------------------------------------------------
# -----------------------------------------------------------------
# CREATE CONVERSION-TABLE DRAFT TO STANDARDIZE GEOGRAPHIC LOCATIONS FROM EACH EXTRACTED DATASET
# INPUT: Raw extracted data files (.RDS)
# OUTPUT: Catalog (.XLSX) with a draft of available geography to be manually edited
# -----------------------------------------------------------------
# -----------------------------------------------------------------
# This code is used to standardize the country/geography codes coming from each source
# Approach is to generate a DRAFT file containing all codes merged in a singles list specifying their origin. This list includes also two new columns (standard code and country name) that will contain the standardized values:
#  Step 1: we generate a draft list in an Excel files called "/userEdition/standardGeography_DRAFT.xlsx"
#  Step 2: an authorized user fill standard unified values for new columns and saves populated list as file as "/userEdition/standardGeography_DRAFT.xlsx"
#  Step 3: list contained in the new file will solve the correspondence between country codes in each original dataset and standard codes
# NOTE: we use an extended meaning of country that combines countrys, nations or even groups of countries. Eg: for FIFA data we get England or Wales, for OECD data we get countries or groups like NAFTA or G7, even for FIFA data, we have a different column for group (UEFA, CONCACAF)
# NOTE: regions/groups of countries are ignored now. In other pieces of code, regions could be used as aggregators, wether for regions already contained in the initial file (eg FIFA), or by using other external table to aggregate single records in current list
# Function to convert locations align field names, add geo field applies
obtainGeodData <- function(df_to_treat, preffix) {
  df_to_treat <- df_to_treat %>% 
    filter(!is.na(countryCode)) %>% 
    group_by(countryCode) %>% 
    summarise(first(date)) %>% 
    mutate(source = preffix, stdCountryCode = "?") %>% 
    select(source, countryCode, stdCountryCode)
  df_to_treat
}
# ===============
# CONSOLIDATE GEO-RELATED DATA
# ---------------
# Create empty dataframe to merge all extracted data
geoDataset <- data.frame(source=character(), countryCode=character(), stdCountryCode=character())
extractedEntities <- sourcesAvailable()  # global function to get all available active data sources
# Process each entity available
for (i in 1:nrow(extractedEntities)) {
  print (paste("Analyzing: ",extractedEntities$Preffix[i],"::",extractedEntities$DataFileName[i]))
  tmpGeoDataset <- obtainGeodData(readRDS(extractedEntities$DataFileName[i]), extractedEntities$Preffix[i])
  geoDataset <- rbind(geoDataset, tmpGeoDataset)
}
head(geoDataset)
# ===============
# DETAIL OF NAMES AND GEOGRAPHIC REGIONS (EXCEPTION)
# ---------------
# Although not used now, we bring from specific sources the name of the countries/regions associated (from FIFA and OECD)
geo_regions_OECD <- readRDS("data/geo_OECD.rds")
geo_regions_OECD <- geo_regions_OECD %>% mutate(source = "OECD", Region = "") %>% select(source, countryCode = LocationId, countryName = LocationName, Region)
extraDetailGeo <- geo_regions_OECD
geoDataset <- geoDataset %>% 
  left_join(extraDetailGeo %>% mutate(stdCountryName="?"), by = c("source", "countryCode")) %>% 
  select(source, countryCode, countryName, regionCode=Region, stdCountryCode, stdCountryName) %>% 
  arrange(countryCode)
# save a DRAFT (standardGeography_DRAFT.xlsx) so an administrator/user can use as reference to generate final file (standardGeography.xlsx)
write.xlsx(geoDataset, "userEdition/standardGeography_DRAFT.xlsx", overwrite = TRUE)


