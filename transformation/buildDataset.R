library(tidyverse)
library(openxlsx)
library(lubridate)  # To create absolute list of dates
library(data.table)  # For dcast() to spread multiple columns
library(reshape2)  # For dcast() to spread multiple columns
library(imputeTS)  # To impute missing values with imputeTS()


# ------------------------------------------------------
# Remove not applying features from raw dataset and filter to seed
# ------------------------------------------------------
# Load consolidated raw dataset, still to refine
fullDataset_raw <- readRDS("data/dataset_raw.rds")
# Extract seed (features and locations to use as hypothesis)
allFeatures_df <- readRDS("data/featuresSeed.rds")
allFeatures_df
# Extract header names of the seed features
seedVbles <- allFeatures_df %>% 
  filter(source != "locations") %>% 
  mutate(vbleName = paste0(source, ".", variable)) %>% 
  pull(vbleName)
seedVbles
# convert country names from seed into standard codes
geoCodesSeed <- allFeatures_df %>% 
  filter(source=="locations") %>% select(variable) %>% 
  left_join(std_geo, by = c("variable" = "countryName")) %>% pull(stdCountryCode)
# filter only geolocations specified in the seed
fullDataset_raw <- fullDataset_raw %>% filter(stdCountryCode %in% c("GLOBAL", geoCodesSeed))
# Reduce features to those in seed and also in actually obtained data
seedVbles <- seedVbles[seedVbles %in% names(fullDataset_raw)]
# Keep remaining columns not forgetting to include date and geo dimensions
seedDataset <- fullDataset_raw %>% select(date, stdCountryCode, seedVbles)

# ------------------------------------------------------
# Spread feature and geo
# ------------------------------------------------------
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

# ------------------------------------------------------
# Save transformed dataset
# ------------------------------------------------------
saveRDS(seedDataset,"data/dataset_seed1.rds")


# ============================================================================
# ============================================================================
# ============================================================================
# ============================================================================


# ------------------------------------------------------
# Imputation (interpolate missing values) and normalization
# ------------------------------------------------------
# Load dataset
seedDataset <- readRDS("data/dataset_seed1.rds")

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
# Function to normalize data to a range 1:100
normalizeTo100 <- function(tsValues) {
  start <- which.min(is.na(tsValues))
  end <- length(tsValues) - which.min(is.na(tsValues[length(tsValues):1]))
  normalizedWithinExistingInterval <- round((100-1)/(max(tsValues[start:end])-min(tsValues[start:end]))*(tsValues[start:end]-max(tsValues[start:end]))+100, digits = 0)
  tsValues[start:end] <- normalizedWithinExistingInterval
  tsValues
}

# Remove columns with <= 2 valid values to avoid imputation issues
empty_columns <- sapply(seedDataset, function(x) sum(!is.na(x)) <= 2)
seedDataset <- seedDataset[, !empty_columns]
# Temporarily remove "date" column to call function that makes massive imputation
tmp_df_noDate <- seedDataset[,!(colnames(seedDataset) == "date")]
# Perform massive imputation to numeric time series (features)
tmp_df_noDate <- as_tibble(apply(tmp_df_noDate, MARGIN=2, FUN=imputeFeatureWithinExistingInterval))




# Perform data normalization to range 1:100
tmp_df_noDate <- as_tibble(apply(tmp_df_noDate, MARGIN=2, FUN=normalizeTo100))

view(seedDataset[1:2000,])



# Add again "date" to processed file
seedDataset <- cbind(date = seedDataset$date, as.data.frame(tmp_df_noDate))
head(seedDataset)
names(seedDataset)
ncol(seedDataset)

# ------------------------------------------------------
# Save transformed dataset
# ------------------------------------------------------
saveRDS(seedDataset,"data/dataset_seed1_proc.rds")







