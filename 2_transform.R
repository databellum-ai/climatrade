
library(tidyverse)

# ===============
# Extract all features and standard geography available
# ---------------
source("transformation/makeCatalogs.R") 
print("SOME FEATURES:")
print(all_features[1:50])
print("SOME DIMENSIONS:")
head(std_geo_revised)
print(std_geo_list[1:50])


# ===============
# Determine what specific data we need to transform
# ---------------
source("load_predictionHyphotesis.R") 


# ===============
# Consolidate required features for their appropriate geography
# ---------------
source("transformation/buildDataset.R") 
