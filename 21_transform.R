
source("10_initialize.R")

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
#       TRANSFORMATION
#
# DATASET REFINEMENT
# Data in .RDS files is transformed, adapted for later use
# INPUT: merged consolidated raw data in .RDS
# OUTPUT: dataset/s in .RDS after customization to seed, outliers removal, imputation and normalization
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(lubridate)  # To create absolute list of dates
library(data.table)  # For dcast() to spread multiple columns
library(reshape2)  # For dcast() to spread multiple columns
library(imputeTS)  # To impute missing values with imputeTS()
# ===============
# LOAD  CONVERSION-TABLE TO STANDARD GEOGRAPHY
# ---------------
# (previously edited/validated by user)
std_geo <- read.xlsx("userEdition/standardGeography.xlsx")
# ===============
# REMOVE FEATURES NOT APPLYING ACCORDING TO SEED
# ---------------
fullDataset_raw <- readRDS("data/dataset_raw.rds")  # Load consolidated raw dataset, still to refine
# Extract header names of the seed features
seedVbles <- seedFeatures_df %>% 
  filter(source != "locations") %>% 
  mutate(vbleName = paste0(source, ".", variable)) %>% 
  pull(vbleName)
seedVbles


# convert country names from seed into standard codes
# geoCodesSeed <- seedFeatures_df %>% 
#   filter(source=="locations") %>% select(variable) %>% 
#   left_join(std_geo, by = c("variable" = "countryName")) %>% pull(stdCountryCode)
# !!:
geoCodesSeed <- seedFeatures_df %>% filter(source=="locations") %>% pull(variable)
geoCodesSeed

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
  # start <- which.min(is.na(tsValues))  # In case we want to do imputation only until most recent KNOWN value and not beyond
  start <- 1
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
# BALANCING AROUND ZERO-REF VALUE
# ---------------
# Some features are always positive, other positive/negative (ref. zero), BUT others have a specific value as reference (for example VIX reference value to distinguish between "good" or "bad" is ~30)
# We now proceed to balance values before normalization

# function to know what substract to each column
valueToSubtractRefZero <- function(columnName) {
  refsZero <- seedFeatures_df %>% filter(type == "measure") %>% mutate(feature = paste0(source,".",variable)) %>% select(feature, refZero)
  refZeroToSubtract <- refsZero$refZero[refsZero$feature == str_split(columnName,"_")[[1]][1]]
  refZeroToSubtract <- refZeroToSubtract[1]
  refZeroToSubtract
}

#using function, we create a numeric vector of values to subtract to each column so we adjust to given "RefZero"
vectorToSubtractRefZero <- sapply(colnames(seedDataset2), valueToSubtractRefZero)
vectorToSubtractRefZero[is.na(vectorToSubtractRefZero)] <- 0
vectorToSubtractRefZero
seedDataset2 <- sweep(seedDataset2, 2, vectorToSubtractRefZero)
# ===============
# NORMALIZATION TO A RANGE
# ---------------
# Load dataset to prepare changes
seedDataset3 <- seedDataset2
# Function to normalize to a -1000 to 1000 range (using only one of the interval extremes)
normalizeTo1000 <- function(tsValues) {
  start <- which.min(is.na(tsValues))
  end <- length(tsValues) - which.min(is.na(tsValues[length(tsValues):1])) + 1
  normalizedWithinExistingInterval <- round(
    (1000-0)/(max(abs(tsValues[start:end]))-min(abs(tsValues[start:end])))*(abs(tsValues[start:end])-max(abs(tsValues[start:end])))+1000, 
    digits = 0)
  tsValues[start:end] <- normalizedWithinExistingInterval * sign(tsValues[start:end])
  tsValues
}
# Temporarily remove "date" column to call function that makes massive normalization
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
# view(seedDataset[1:2000,46:80])
# view(seedDataset2[1:2000,46:80])
# view(seedDataset3[1:2000,46:80])
write.xlsx(seedDataset, "data/dataset_seed1_p1.xlsx")
write.xlsx(seedDataset2, "data/dataset_seed1_p2.xlsx")
write.xlsx(seedDataset3, "data/dataset_seed1_p3.xlsx")

print("FINISHED transformation process")
