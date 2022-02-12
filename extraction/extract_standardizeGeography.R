
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


# ------------------------------------------------------
# Convert locations align field names, add geo field applies
# ------------------------------------------------------

# Function to treat received datasets from extraction
# Convert locations align field names, add geo field applies
obtainGeodData <- function(df_to_treat, preffix) {
  df_to_treat <- df_to_treat %>% 
    filter(!is.na(countryCode)) %>% 
    group_by(countryCode) %>% 
    summarise(first(date)) %>% 
    mutate(source = preffix, stdCountryCode = "?") %>% 
    select(source, countryCode, stdCountryCode)
  df_to_treat
}

# ------------------------------------------------------
# Load extracted data, treat to homogenize and merge all
# ------------------------------------------------------
# Create empty dataframe to merge all extracted data
geoDataset <- data.frame(source=character(), countryCode=character(), stdCountryCode=character())

# Process each entity available
for (i in 1:nrow(extractedEntities)) {
  print (paste("Analyzing: ",extractedEntities$Preffix[i],"::",extractedEntities$DataFileName[i]))
  tmpGeoDataset <- obtainGeodData(readRDS(extractedEntities$DataFileName[i]), extractedEntities$Preffix[i])
  geoDataset <- rbind(geoDataset, tmpGeoDataset)
}

head(geoDataset)

# save a DRAFT (standardGeography_DRAFT.xlsx) so an administrator/user can use as reference to generate final file (standardGeography.xlsx)
write.xlsx(geoDataset, "userEdition/standardGeography_DRAFT.xlsx", overwrite = TRUE)



