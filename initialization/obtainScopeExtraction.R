# ------------------------------------------------------
# ------------------------------------------------------
 # INPUT: .XLSX user edited defining the variables to extract
 # OUTPUT: .RDS containing a dataframe with the structured scope (hypothesis)
# ------------------------------------------------------

library(tidyverse)
library(openxlsx)

scope <- read.xlsx("userEdition/ScopeExtraction.xlsx")
names(scope)

goalFeatures <- scope$FeatureCode
searchTerms <- c(specificSearchTerms, genericSearchTerms)
  specificSentimentTerms <- scope$SpecificSentimentTerms[!(is.na(scope$SpecificSentimentTerms))]
  genericSentimentTerms <- strsplit(scope$GenericSentimentTerms[1], "\n")[[1]]
sentimentTerms <- c(specificSentimentTerms, genericSentimentTerms)
cities <- scope %>% filter(!is.na(Cities)) %>% pull(Cities)
addresses <- scope %>% filter(!is.na(Addresses)) %>% pull(Addresses)

goalFeatures
searchTerms
sentimentTerms
cities
addresses

allFeatures_df <- 
  rbind(data.frame(source="stocks", variable=goalFeatures, type="outcome"),
    data.frame(source="searchesGoogle", variable=searchTerms, type="prescriptor"),
    data.frame(source="twitterSentiment", variable=sentimentTerms, type="prescriptor")) %>% 
      mutate(
        termsDetailed = str_extract(variable,  "(?<=\\().+?(?=\\))"), 
        termsDetailed = replace(termsDetailed, is.na(termsDetailed), ""), 
        variable = str_remove(variable, paste0("\\(",termsDetailed,"\\)"))) %>% 
  select(source, variable, termsDetailed, type) %>% 
  rbind(data.frame(source="cities", variable=cities, termsDetailed=addresses, type="city"))

allFeatures_df

# ------------------------------------------------------
# Save scope converted to dataframe to use during Extraction and Transformation
# ------------------------------------------------------
saveRDS(allFeatures_df,"data/scopeExtraction.rds")

