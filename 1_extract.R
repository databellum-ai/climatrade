
library(tidyverse)
library(openxlsx)
library(lubridate)
library(ggthemes)


# ===============
# KEYS
# ---------------
source("./extraction/keys_APIs.R")


# ===============
# KNOW WHAT DATA WE NEED TO EXTRACT
# ---------------
source("load_extractionScope.R")


# ===============
# DATA BACKUP BEFORE NEW EXTRACTION
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


# -----------------------------------------------------------------
# -----------------------------------------------------------------
# Generate editable geography codes proposal and read its revised (manually edited) version
# -----------------------------------------------------------------
# -----------------------------------------------------------------
source("extraction/extract_standardizeGeography.R") # Prepare a standard geography proposal ("userEdition/standardGeography_DRAFT.xlsx") coding to mix data from disparate sources
# NOW users edits the draft and saves as "userEdition/standardGeography.xlsx":
std_geo <- read.xlsx("userEdition/standardGeography.xlsx") # Read user-edited version ("userEdition/standardGeography.xlsx") 



# -----------------------------------------------------------------
# -----------------------------------------------------------------
# EDA
# -----------------------------------------------------------------
# -----------------------------------------------------------------
# Google searches chart
all_searches %>% group_by(KAM) %>%
  ggplot(aes(x = date, y = nSearches, color = date)) +
  geom_point(size = 1) +
  facet_wrap(~KAM) +
  ggthemes::theme_economist() + labs(title = "Interest over Time", subtitle = "Google Trends Report", caption = "By databellum®")



# Stocks price chart
stocksData %>%
  ggplot(aes(x = date, y = value, color = symbol)) +
  geom_line() +
  facet_wrap(~symbol,scales = 'free_y') +
  theme_classic() +
  labs(x = 'Date',
       y = "Close Price",
       title = "Stocks Chart", subtitle = "", caption = "By databellum®") +
  scale_x_date(date_breaks = "month",
               date_labels = "%b\n%y") + 
  ggthemes::theme_economist()
# Stocks volume chart
stocksVolume %>%
  ggplot(aes(x = date, y = value, color = symbol)) +
  geom_line() +
  facet_wrap(~symbol,scales = 'free_y') +
  theme_classic() +
  labs(x = 'Date',
       y = "Volume",
       title = "Stocks", subtitle = "", caption = "By databellum®") +
  scale_x_date(date_breaks = "month",
               date_labels = "%b\n%y") + 
  ggthemes::theme_economist()


# OECD chart
leadingIndicatorsOECD %>%
  ggplot(aes(as_date(Date), OECD_CLI, color=Country)) + geom_line()



