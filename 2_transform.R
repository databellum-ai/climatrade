library(tidyverse)


source("transformation/makeFeaturesCatalog.R") # Extract all available features (excluding geography)
source("transformation/makeGeographyCatalog.R") # Extract all available standardized countries

# Now, we prepare the table/network of features to extract and each associated geography

source("transformation/buildDataset.R") # consolidate required features for their appropriate geography
