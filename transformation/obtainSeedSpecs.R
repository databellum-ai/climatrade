# ------------------------------------------------------
# ------------------------------------------------------
# Convert table of "seed" tables (hypothesis) from .XLSX to a structured dataframe and store as .RDS
# Since this this dataframe identifies exactly what features and standard geolocations will be used as "seed", it will be later used during Extraction and Transformation phases
 # INPUT: .XLSX user edited defining the seed (hypothesis)
 # OUTPUT: .RDS containing a dataframe with the structured seed (hypethesis)
# ------------------------------------------------------

library(tidyverse)
library(openxlsx)

seed <- read.xlsx("userEdition/seed1.xlsx")
names(seed)

goalFeatures <- seed$FeatureCode
goalFeatureNames <- seed$FeatureName
moodFeatures <- as.data.frame(do.call(rbind, strsplit(strsplit(seed$PlanetMoodFeatures[1], "\n")[[1]], "\\."))) %>% rename(source = V1, variable = V2)
  specificSearchTerms <- seed$SpecificSearchTerms[!(is.na(seed$SpecificSearchTerms))]
  genericSearchTerms <- strsplit(seed$GenericSearchTerms[1], "\n")[[1]]
searchTerms <- c(specificSearchTerms, genericSearchTerms)
  specificSentimentTerms <- seed$SpecificSentimentTerms[!(is.na(seed$SpecificSentimentTerms))]
  genericSentimentTerms <- strsplit(seed$GenericSentimentTerms[1], "\n")[[1]]
sentimentTerms <- c(specificSentimentTerms, genericSentimentTerms)
  specificStd_Geo <- seed$SpecificStd_Geo[!(is.na(seed$SpecificStd_Geo))]
  genericStd_Geo <- strsplit(seed$GenericStd_Geo[1], "\n")[[1]]
geoLocations <- c(specificStd_Geo, genericStd_Geo)

goalFeatureNames
goalFeatures
moodFeatures
searchTerms
sentimentTerms
geoLocations

allFeatures_df <- 
  rbind(
    data.frame(source="stocks", variable=goalFeatures, type="outcome"),
    data.frame(moodFeatures, type="prescriptor"),
    data.frame(source="searchesGoogle", variable=searchTerms, type="prescriptor"),
    data.frame(source="twitterSentiment", variable=sentimentTerms, type="prescriptor"), 
    data.frame(source="locations", variable=geoLocations, type="dimension")) %>%
  mutate(
    termsDetailed = str_extract(variable,  "(?<=\\().+?(?=\\))"), 
    termsDetailed = replace(termsDetailed, is.na(termsDetailed), ""), 
    variable = str_remove(variable, paste0("\\(",termsDetailed,"\\)")
    )) %>% 
  select(source, variable, termsDetailed, type)

allFeatures_df

# ------------------------------------------------------
# Save seed converted to dataframe to use during Extraction and Transformation
# ------------------------------------------------------
saveRDS(allFeatures_df,"data/seedSpecs.rds")

