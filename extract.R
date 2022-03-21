# PTE: probar/automatizar proceso Extract de GDELT/BigQuery (oauth, etc)
# PTE: IAI: comprobar captura directa CSV

source("initialize.R")

library(tidyverse)
library(openxlsx)

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
#       EXTRACTION AND TRANSFORMATION
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
#       EXTRACTION DATA FROM SOURCES
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# RAW DATA EXTRACTION
# INPUT: several internet sources, APIs, etc.
# OUTPUT: .RDS files from each source
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ===============
# KEYS
# ---------------
source("keys_APIs.R")
# ===============
# DATA BACKUP
# ---------------
# We use a new directory named by date and time to store current .RDS files, etc. (all content)
library(stringr)
dir_from <- "data"
dir_to <- str_remove_all(paste0("dataExtracted_", as.character(Sys.time())), "[-: ]")
dir_to <- file.path("backup", dir_to)
dir.create(dir_to)
file.copy(list.files(dir_from, full.names = TRUE), dir_to, recursive = TRUE)
# ===============
# RUN DATA EXTRACTION (APIS, ETC.)
# ---------------
extractedEntities <- sourcesAvailable()
extractedEntities
for (i in 1:nrow(extractedEntities)) {
  print("=======================================================================")
  print("=======================================================================")
  print(paste0("=======>> RUNNING EXTRACTION: ", extractedEntities$Description[i], " (", extractedEntities$SourceCode[i], ")"))
  source(extractedEntities$SourceCode[i])
}


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# MERGE RAW DATA
# Merge all extracted data (still to refine) into a single raw file, 
# INPUT: .RDS files from each source
# OUTPUT: .RDS merged file (still raw, no seed applied)
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
library(lubridate)  # To create absolute list of dates
# ===============
# LOAD  CONVERSION-TABLE TO STANDARD GEOGRAPHY
# ---------------
# (previously edited/validated by user)
std_geo <- read.xlsx("userEdition/standardGeography.xlsx")
reduced_std_geo <- std_geo %>% select(source, countryCode, stdCountryCode)
head(reduced_std_geo)
# ------------------------------------------------------
# FUNCTION TO ADD PREFFIX AND CONVERT TO STANDARD LOCATION
# ------------------------------------------------------
uniformExtractedData <- function(df_to_treat, preffix) {
  if (preffix == "music") {
    df_to_treat <- df_to_treat %>% select(-country)# EXCEPTION: although brought in data extracted, we do not need now some columns.
  }
  names(df_to_treat) <- paste0(preffix,".", names(df_to_treat))  # add source-related preffix
  df_to_treat <- df_to_treat %>% 
    rename(date = paste0(preffix,".date"), countryCode = paste0(preffix,".countryCode")) %>% 
    mutate(countryCode = toupper(countryCode))
  df_to_treat <- df_to_treat %>% 
    left_join((reduced_std_geo %>% filter(source == preffix)), by = "countryCode") %>% 
    select(-c("countryCode", "source"))
  df_to_treat
}
# ===============
# MERGE ALL AVAILABLE RAW DATA
# ---------------
# Create empty dataframe to merge all extracted data
fullDataset_raw <- data.frame(date=as.Date(character()), stdCountryCode=character())
# Process each entity available

for (i in 1:nrow(extractedEntities)) {
  print (paste("Merging: ",extractedEntities$Preffix[i],"::",extractedEntities$DataFileName[i]))
  fullDataset_raw <- merge(fullDataset_raw, uniformExtractedData(readRDS(extractedEntities$DataFileName[i]), extractedEntities$Preffix[i]), by = c("date", "stdCountryCode"), all=TRUE)
}
# NA values in countryCode indicate "GLOBAL" scope for those features
fullDataset_raw <- fullDataset_raw %>% mutate(stdCountryCode = ifelse(is.na(stdCountryCode),"GLOBAL",stdCountryCode))


# ===============
# SAVE CONSOLIDATED RAW DATASET, STILL TO REFINE
# ---------------
saveRDS(fullDataset_raw,"data/dataset_raw.rds")
print("All data merged and stored")


