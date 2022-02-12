library(tidyverse)
library(openxlsx)
library(lubridate)  # To create absolute list of dates


# ------------------------------------------------------
# Convert locations align field names, add geo field applies
# ------------------------------------------------------

# Function to treat received datasets from extraction
# Convert locations align field names, add geo field applies
uniformExtractedData <- function(df_to_treat, preffix) {
  if (preffix == "music") {
    df_to_treat <- df_to_treat %>% select(-country)# EXCEPTION: although brought in data extracted, we do not need now some columns.
  }
  names(df_to_treat) <- paste0(preffix,".", names(df_to_treat))
  df_to_treat <- df_to_treat %>% 
    rename(date = paste0(preffix,".date"), countryCode = paste0(preffix,".countryCode")) %>% 
    mutate(countryCode = toupper(countryCode))
  df_to_treat <- df_to_treat %>% 
    left_join((reduced_std_geo %>% 
                 filter(source == preffix)), by = "countryCode") %>% 
    select(-c("countryCode", "source"))
  df_to_treat
}

# ------------------------------------------------------
# Load standard geography (previously validated by user)
std_geo <- read.xlsx("userEdition/standardGeography.xlsx")
head(std_geo)
# Get standard geo codes and align field names
reduced_std_geo <- std_geo %>% select(source, countryCode, stdCountryCode)
head(reduced_std_geo)

# ------------------------------------------------------
# Load extracted data, treat to homogenize and merge all
# ------------------------------------------------------
# Create empty dataframe to merge all extracted data
fullDataset_raw <- data.frame(date=as.Date(character()), stdCountryCode=character())
# Process each entity available
for (i in 1:nrow(extractedEntities)) {
  print (paste("Merging: ",extractedEntities$Preffix[i],"::",extractedEntities$DataFileName[i]))
  fullDataset_raw <- merge(fullDataset_raw, uniformExtractedData(readRDS(extractedEntities$DataFileName[i]), extractedEntities$Preffix[i]), by = c("date", "stdCountryCode"), all=TRUE)
}
# NA values in countryCode indicate "GLOBAL" scope for those features
fullDataset_raw <- fullDataset_raw %>% mutate(stdCountryCode = ifelse(is.na(stdCountryCode),"GLOBAL",stdCountryCode))



# ------------------------------------------------------
# Save consolidated raw dataset, still to refine
# ------------------------------------------------------
saveRDS(fullDataset_raw,"data/dataset_raw.rds")

