# Fusionar con "initialization/load_extractionScope.R"

library(tidyverse)


# ------------------------------------------------------
# Extract seed (features and locations to use as hypothesis)
# ------------------------------------------------------

seed1 <- read.xlsx("docs/seed1.xlsx")
head(seed1)
names(seed1)

goalFeatures <- seed1$FeatureCode
goalFeatureNames <- seed1$FeatureName
moodFeatures <- strsplit(seed1$PlanetMoodFeatures[1], "\n")[[1]]
specificSearchConcepts <- seed1$SpecificSearchConcepts[!(is.na(seed1$SpecificSearchConcepts))]
genericSearchConcepts <- strsplit(seed1$GenericSearchConcepts[1], "\n")[[1]]
searchConcepts <- c(specificSearchConcepts, genericSearchConcepts)
specificSentimentConcepts <- seed1$SpecificSentimentConcepts[!(is.na(seed1$SpecificSentimentConcepts))]
genericSentimentConcepts <- strsplit(seed1$GenericSentimentConcepts[1], "\n")[[1]]
sentimentConcepts <- c(specificSentimentConcepts, genericSentimentConcepts)
specificStd_Geo <- seed1$SpecificStd_Geo[!(is.na(seed1$SpecificStd_Geo))]
genericStd_Geo <- strsplit(seed1$GenericStd_Geo[1], "\n")[[1]]
geoLocations <- c(specificStd_Geo, genericStd_Geo)

goalFeatures
moodFeatures
searchConcepts
sentimentConcepts
geoLocations


# ------------------------------------------------------
# Load standard geography (previously validated by user)
# ------------------------------------------------------

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






