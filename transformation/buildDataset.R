# Fusionar con "initialization/load_extractionScope.R"

library(tidyverse)
library(openxlsx)

# ------------------------------------------------------
# Extract seed (features and locations to use as hypothesis)
# ------------------------------------------------------

seed1 <- read.xlsx("docs/seed1.xlsx")
head(seed1)
names(seed1)

goalFeatures <- seed1$FeatureCode
goalFeatureNames <- seed1$FeatureName
moodFeatures <- as.data.frame(do.call(rbind, strsplit(strsplit(seed1$PlanetMoodFeatures[1], "\n")[[1]], "\\."))) %>% rename(source = V1, feature = V2)
  specificSearchTerms <- seed1$SpecificSearchTerms[!(is.na(seed1$SpecificSearchTerms))]
  genericSearchTerms <- strsplit(seed1$GenericSearchTerms[1], "\n")[[1]]
searchTerms <- c(specificSearchTerms, genericSearchTerms)
  specificSentimentTerms <- seed1$SpecificSentimentTerms[!(is.na(seed1$SpecificSentimentTerms))]
  genericSentimentTerms <- strsplit(seed1$GenericSentimentTerms[1], "\n")[[1]]
sentimentTerms <- c(specificSentimentTerms, genericSentimentTerms)
  specificStd_Geo <- seed1$SpecificStd_Geo[!(is.na(seed1$SpecificStd_Geo))]
  genericStd_Geo <- strsplit(seed1$GenericStd_Geo[1], "\n")[[1]]
geoLocations <- c(specificStd_Geo, genericStd_Geo)

goalFeatureNames
goalFeatures
moodFeatures
searchTerms
sentimentTerms
geoLocations

allFeatures_df <- rbind(
  data.frame(source="stocks", feature=goalFeatures, isGoal=TRUE),
  data.frame(moodFeatures, isGoal=FALSE) ,
  data.frame(source="searchesGoogle", feature=searchTerms, isGoal=FALSE),
  data.frame(source="twitterSentiment", feature=sentimentTerms, isGoal=FALSE)
) %>%
  mutate(
    termsDetailed = str_extract_all(feature,  "(?<=\\().+?(?=\\))"),
    feature2 = str_extract_all(feature, "^\\w+$|^.+(?=\\(.?)"))

allFeatures_df



# About regular expressions:
# https://cran.r-project.org/web/packages/stringr/vignettes/regular-expressions.html

str_split(allFeatures_df$feature, "\\(")

str_extract_all("canada(from:canadianPM, trudeau)",  "^\\w+$|^.+(?=\\(.?)")
str_extract_all("NG=F", "\\w+$")
str_extract_all("canada", "\\w+$")
str_extract_all("san sebastian", "\\w+$")

c <-  "ce(7382)"
gsub("[^.?\\(]", "", c)
gsub("lo", "", "eloy")

test <- "canada(from:canadianPM, trudeau)"
str_extract_all(test, "(?<=\\().+?(?=\\))")
str_split(test, "\\(")[[1]][2]






# ------------------------------------------------------
# Load standard geography (previously validated by user)
# ------------------------------------------------------

geoLocations

std_geo <- read.xlsx("userEdition/standardGeography.xlsx")
head(std_geo)








# ------------------------------------------------------
# Get data from extracted .RDS files
# ------------------------------------------------------

airTraffic <- readRDS("data/data_airTraffic_ts.rds")
FIFA <- readRDS("data/data_FIFA_ts.rds")
moonSun <- readRDS("data/data_moonSun_ts.rds")
music <- readRDS("data/data_music_ts.rds")
OECD <- readRDS("data/data_OECD_ts.rds")
searchesGoogle <- readRDS("data/data_searchesGoogle_ts.rds")
twitterSentiment <- readRDS("data/data_twitterSentiment_ts.rds")
stocks <- readRDS("data/data_stocks_ts.rds")

head(airTraffic)
head(FIFA)
head(moonSun)
head(music)
head(OECD)
head(searchesGoogle)
head(twitterSentiment)
head(stocks)


# ------------------------------------------------------
# Build a dataset based specifically in the seed (hypothesis)
# ------------------------------------------------------






