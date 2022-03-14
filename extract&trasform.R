## Probar proceso completo Extraction y Transformation (AÑADIENDO index, GDELT + QUITANDO moonSum, FIFA, twitter)
# Terminar index (IAI): leer directamente el .csv + obtener datos históricos


source("initialize.R")

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
#       EXTRACTION AND TRANSFORMATION
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# OBTAIN SEED SPECIFICATIONS
# INPUT: .XLSX user edited defining the seed (hypothesis)
# OUTPUT: dataframe with the structured seed (hypethesis)
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Establish what specific stocks, features, concepts and geography locations we want to analize for a given seed
# Determine what stocks, features, concepts and geography locations we want to analize.
# Code convert table of "seed" tables (hypothesis) from .XLSX to a structured dataframe
# Since this this dataframe identifies exactly what features and standard geolocations will be used as "seed", it will be later used during Extraction and Transformation phases
seed <- read.xlsx("userEdition/seed1.xlsx")
names(seed)
goalFeatures <- seed$FeatureCode[!is.na(seed$FeatureCode)]
goalFeatureNames <- seed$FeatureName[!is.na(seed$FeatureName)]
moodFeatures <- as.data.frame(do.call(rbind, strsplit(seed$PlanetMoodFeatures, "\\."))) %>% rename(source = V1, variable = V2)
moodFeatures <- moodFeatures[!(is.na(seed$PlanetMoodFeatures)),]
SearchTerms <- seed$SearchTerms[!(is.na(seed$SearchTerms))]
Events <- seed$Events[!(is.na(seed$Events))]
Std_Geo <- seed$Std_Geo[!(is.na(seed$Std_Geo))]
geoLocations <- Std_Geo
seedFeatures_df <- 
  rbind(
    data.frame(source="stocks", variable=goalFeatures, type="outcome"),
    data.frame(moodFeatures, type="prescriptor"),
    data.frame(source="searchesGoogle", variable=searchTerms, type="prescriptor"),
    data.frame(source="locations", variable=geoLocations, type="dimension")) %>%
  mutate(
    termsDetailed = str_extract(variable,  "(?<=\\().+?(?=\\))"), 
    termsDetailed = replace(termsDetailed, is.na(termsDetailed), ""), 
    variable = str_remove(variable, paste0("\\(",termsDetailed,"\\)"))) %>% 
  select(source, variable, termsDetailed, type)
seedFeatures_df


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
#       EXTRACTION DATA FROM SOURCES
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

library(tidyverse)
library(openxlsx)

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
codeToRunExtraction <- sourcesAvailable()
codeToRunExtraction
for (i in 1:nrow(codeToRunExtraction)) {
  print(paste0("Running extraction: ", codeToRunExtraction$Description[i], " (", codeToRunExtraction$SourceCode[i], ")"))
  source(codeToRunExtraction$SourceCode[i])
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



# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
#       TRANSFORMATION
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

library(tidyverse)
library(openxlsx)

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# DATASET REFINE
# Data in .RDS files is transformed, adapted for later use
# INPUT: merged consolidated raw data in .RDS
# OUTPUT: dataset/s in .RDS after customization to seed, outliers removal, imputation and normalization
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
library(lubridate)  # To create absolute list of dates
library(data.table)  # For dcast() to spread multiple columns
library(reshape2)  # For dcast() to spread multiple columns
library(imputeTS)  # To impute missing values with imputeTS()
# ===============
# REMOVE FEATURES NOT APPLYING ACCORDING TO SEED
# ---------------
fullDataset_raw <- readRDS("data/dataset_raw.rds")  # Load consolidated raw dataset, still to refine
seedFeatures_df
# Extract header names of the seed features
seedVbles <- seedFeatures_df %>% 
  filter(source != "locations") %>% 
  mutate(vbleName = paste0(source, ".", variable)) %>% 
  pull(vbleName)
seedVbles
# convert country names from seed into standard codes
geoCodesSeed <- seedFeatures_df %>% 
  filter(source=="locations") %>% select(variable) %>% 
  left_join(std_geo, by = c("variable" = "countryName")) %>% pull(stdCountryCode)
# filter only geolocations specified in the seed
fullDataset_raw <- fullDataset_raw %>% filter(stdCountryCode %in% c("GLOBAL", geoCodesSeed))
# Reduce features to those in seed and also in actually obtained data
seedVbles <- seedVbles[seedVbles %in% names(fullDataset_raw)]
# Keep remaining columns not forgetting to include date and geo dimensions
seedDataset <- fullDataset_raw %>% select(date, stdCountryCode, seedVbles)
# ===============
# SPREAD FEATURE AND _GEO
# ---------------
# Spread geo locations for each one of the features
seedDataset <- melt(seedDataset, id.vars = c("date", "stdCountryCode"))
# Remove combinations with NA value and convert existing values into numeric
seedDataset <- seedDataset %>% 
  filter(!is.na(value)) %>% 
  mutate(value = as.numeric(value))
# Spread with existing geolocation as suffix
seedDataset <- reshape2::dcast(
  seedDataset, date ~ variable + stdCountryCode, 
  fun.aggregate = function(x) if(length(x) == 0) NA_real_ else sum(x, na.rm = TRUE))
# Ensure there are records for all possible dates, even if they have no value o any feature
allAbsoluteDates <- as_tibble(as.Date(seq(ymd(absoluteInitialDate, tz = "UTC"), as.POSIXct(Sys.Date()), by="days")))
colnames(allAbsoluteDates) <- c("date")
seedDataset <- seedDataset %>% 
  right_join(allAbsoluteDates, by = "date")
seedDataset <- seedDataset %>% arrange(desc(date))
head(seedDataset)
# ===============
# IMPUTATION
# ---------------
# Load dataset to prepare changes
seedDataset2 <- seedDataset
# Function to detect start and end of time serie values and apply interpolation to fill missing values (NAs)
# Missing Values Very recent (nor yet extracted) or very old (before historic availability) will remain NA 
# On imputation function: https://cran.r-project.org/web/packages/imputeTS/imputeTS.pdf
imputeFeatureWithinExistingInterval <- function(tsValues) {
  start <- which.min(is.na(tsValues))
  end <- length(tsValues) - which.min(is.na(tsValues[length(tsValues):1]))
  imputedWithinExistingInterval <- na_interpolation(tsValues[start:end])
  tsValues[start:end] <- imputedWithinExistingInterval
  tsValues
}
# Function to remove outliers based in 1.5 times distance between percentiles 1 and 99 (pretty conservative)
# For these outliers, value is moved to NA, and it will be in next step imputated (linearly)
removeOutliers <- function(tsValues) {
  Q <- quantile(tsValues, probs=c(.001, .999), na.rm = TRUE)
  iqr <- IQR(tsValues, na.rm = TRUE)
  low <- Q[1]-1.5*iqr # Lower Range
  up <-  Q[2]+1.5*iqr # Upper Range
  tsValues <- ifelse(!(between(tsValues,low,up)),NA,tsValues)
  tsValues
}
# Remove columns with <= 2 valid values to avoid imputation issues
empty_columns <- sapply(seedDataset2, function(x) sum(!is.na(x)) <= 2)
seedDataset2 <- seedDataset2[, !empty_columns]
# Temporarily remove "date" column to call functions that remove outliers and make massive imputation
tmp_df_noDate <- seedDataset2[,!(colnames(seedDataset2) == "date")]
# Perform outliers removal
tmp_df_noDate <- as_tibble(apply(tmp_df_noDate, MARGIN=2, FUN=removeOutliers))
# Perform massive imputation to numeric time series (features)
tmp_df_noDate <- as_tibble(apply(tmp_df_noDate, MARGIN=2, FUN=imputeFeatureWithinExistingInterval))
seedDataset2 <- cbind(date = seedDataset2$date, as.data.frame(tmp_df_noDate)) # Add again "date" to processed file
# ===============
# NORMALIZATION TO A RANGE
# ---------------
# Load dataset to prepare changes
seedDataset3 <- seedDataset2
# Function to nomalize to a -1000 to 1000 range (using only one of the interval extremes)
normalizeTo1000 <- function(tsValues) {
  start <- which.min(is.na(tsValues))
  end <- length(tsValues) - which.min(is.na(tsValues[length(tsValues):1])) + 1
  normalizedWithinExistingInterval <- round(
    (1000-0)/(max(abs(tsValues[start:end]))-min(abs(tsValues[start:end])))*(abs(tsValues[start:end])-max(abs(tsValues[start:end])))+1000, 
    digits = 0)
  tsValues[start:end] <- normalizedWithinExistingInterval * sign(tsValues[start:end])
  tsValues
}
# Temporarily remove "date" column to call function that makes massive imputation
tmp_df_noDate <- seedDataset3[,!(colnames(seedDataset3) == "date")]
# Perform data normalization to range 1:100
tmp_df_noDate <- as_tibble(apply(tmp_df_noDate, MARGIN=2, FUN=normalizeTo1000))
seedDataset3 <- cbind(date = seedDataset3$date, as.data.frame(tmp_df_noDate)) # Add again "date" to processed file
# ===============
# SAVE TRANSFORMED DATASETS
# ---------------
saveRDS(seedDataset,"data/dataset_seed1_p1.rds")  # Dataset with original values customized to the existing seed and spread to final columns format
saveRDS(seedDataset2,"data/dataset_seed1_p2.rds")  # Dataset including imputation of missing values and removing empty columns
saveRDS(seedDataset3,"data/dataset_seed1_p3.rds")  # Dataset adding conversion to 1:1000 range
# view(seedDataset[1:2000,1:45])
# view(seedDataset2[1:2000,1:45])
# view(seedDataset3[1:2000,1:45])
write.xlsx(seedDataset, "data/dataset_seed1_p1.xlsx")
write.xlsx(seedDataset2, "data/dataset_seed1_p2.xlsx")
write.xlsx(seedDataset3, "data/dataset_seed1_p3.xlsx")



