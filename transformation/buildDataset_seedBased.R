# - OUTLIER-STOCKS: (>ABS(10x)/día): eliminar dato (antes de generar _p1)

library(tidyverse)
library(openxlsx)
library(lubridate)  # To create absolute list of dates
library(data.table)  # For dcast() to spread multiple columns
library(reshape2)  # For dcast() to spread multiple columns
library(imputeTS)  # To impute missing values with imputeTS()

# ------------------------------------------------------
# Remove not applying features from raw dataset and filter to seed
# ------------------------------------------------------
fullDataset_raw <- readRDS("data/dataset_raw.rds")  # Load consolidated raw dataset, still to refine
allFeatures_df <- readRDS("data/featuresSeed.rds")  # Extract seed (features and locations to use as hypothesis)
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
# Imputation (interpolate missing values) and normalization
# ------------------------------------------------------
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

# Remove columns with <= 2 valid values to avoid imputation issues
empty_columns <- sapply(seedDataset2, function(x) sum(!is.na(x)) <= 2)
seedDataset2 <- seedDataset2[, !empty_columns]
# Temporarily remove "date" column to call function that makes massive imputation
tmp_df_noDate <- seedDataset2[,!(colnames(seedDataset2) == "date")]
# Perform massive imputation to numeric time series (features)
tmp_df_noDate <- as_tibble(apply(tmp_df_noDate, MARGIN=2, FUN=imputeFeatureWithinExistingInterval))
seedDataset2 <- cbind(date = seedDataset2$date, as.data.frame(tmp_df_noDate)) # Add again "date" to processed file





test <- c(25,35,260,26,28,27)

test <- tmp_df_noDate$'stocks.JPYEUR=X_GLOBAL'[1920:1925]
test
ifelse(c(1,diff(test))==0,0,1)

ifelse( (abs(test-lag(test))) %in% c(NA, FALSE), test-lag(test), 0)

test
diff(lag = 1, test)
diff(lag = 2, test)






# ------------------------------------------------------
# Normalization to a range
# ------------------------------------------------------
# Load dataset to prepare changes
seedDataset3 <- seedDataset2

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


# ------------------------------------------------------
# Save transformed datasets
# ------------------------------------------------------
saveRDS(seedDataset,"data/dataset_seed1_p1.rds")  # Dataset with original values customized to the existing seed and spread to final columns format
saveRDS(seedDataset2,"data/dataset_seed1_p2.rds")  # Dataset including imputation of missing values and removing empty columns
saveRDS(seedDataset3,"data/dataset_seed1_p3.rds")  # Dataset adding conversion to 1:1000 range


# view(seedDataset[1:2000,170:180])
view(seedDataset[1:2000,175:179])
view(seedDataset2[1:2000,175:179])
view(seedDataset3[1:2000,175:179])
write.xlsx(seedDataset, "data/dataset_seed1_p1.xlsx")
write.xlsx(seedDataset2, "data/dataset_seed1_p2.xlsx")
write.xlsx(seedDataset3, "data/dataset_seed1_p3.xlsx")



