
library(tidyverse)
library(openxlsx)
library(lubridate)
library(ggthemes)


# ===============
# KEYS
# ---------------
source("keys_APIs.R")


# ===============
# Data backup before new extraction
# ---------------
# We use a new directory named by date and time to store current .RDS files, etc. (all content)
dir_from <- "data"
dir_to <- str_remove_all(paste0("dataExtracted_", as.character(Sys.time())), "[-: ]")
dir_to <- file.path("backup", dir_to)
dir.create(dir_to)
file.copy(list.files(dir_from, full.names = TRUE), 
          dir_to, 
          recursive = TRUE)


# -----------------------------------------------------------------
# -----------------------------------------------------------------
# Data extraction from miscelaneous sources
# -----------------------------------------------------------------
# -----------------------------------------------------------------
source("extraction/extract_searchsGTrends.R")# Extract searches from Google Trends
source("extraction/extract_stocksPrices.R") # Extract stock prices from Yahoo Finance
source("extraction/extract_weatherNOAA.R")# Extract weather from NOAA
source("extraction/extract_airTraffic.R")# Extract air traffic data
source("extraction/extract_indicatorsOECD.R")# Extract leading indicators from OECD
source("extraction/extract_moonSunData.R")# Extract Moon and Sun related data (phase, night hours)
source("extraction/extract_sentimentsTwitter.R")# Extract Twitter posts sentiment data
source("extraction/extract_rankingFIFA.R")# Extract from FIFA Ranking
source("extraction/extract_musicSPOTIFY.R") # Extract music trends from SPOTIFY




