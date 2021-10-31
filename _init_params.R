if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(tidyquant)) install.packages("tidyquant", repos = "http://cran.us.r-project.org") # To get stock data
if(!require(gtrendsR)) install.packages("gtrendsR", repos = "http://cran.us.r-project.org") # To get GoogleTrends data
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org") # To get GoogleTrends data
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(jsonlite)) install.packages("jsonlite", repos = "http://cran.us.r-project.org")
if(!require(spotifyr)) install.packages("spotifyr", repos = "http://cran.us.r-project.org")
if(!require(rnoaa)) install.packages("rnoaa", repos = "http://cran.us.r-project.org")
if(!require(OECD)) install.packages("OECD", repos = "http://cran.us.r-project.org")
if(!require(gtools)) install.packages("gtools", repos = "http://cran.us.r-project.org")
if(!require(ggmaps)) install.packages("ggmaps", repos = "http://cran.us.r-project.org") # obtain city coordianates

library(tidyquant)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(tidyquant) # To get stock data
library(gtrendsR) # To get GoogleTrends data
library(ggthemes)
library(rvest)
library(jsonlite)
library(spotifyr)# For certain functions and applications, you’ll need to log in as a Spotify user. To do this, your Spotify Developer application needs to have a callback url. You can set this to whatever you want that will work with your application, but a good default option is http://localhost:1410/ (see image below). For more information on authorization, visit the offical Spotify Developer Guide. (https://www.rcharlie.com/spotifyr/)
  Sys.setenv(SPOTIFY_CLIENT_ID = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx')
  Sys.setenv(SPOTIFY_CLIENT_SECRET = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx')
  # source("clavesAPI_spotify.R")
library(readr)
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(rnoaa)
  options(noaakey = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
  # source("clavesAPI_noaa.R")
library(OECD) # To get OECD.org indicators
library(gtools)

# ================================
# Parameters
# ================================
#
# Prices parameters:
chosenTickers = c("AAPL", "NFLX", "AMZN", "AAIC", "MSFT", "AAN")
endDateTicker = as.Date(today())
startingDateTicker = endDateTicker - 365 # also as... "2021-01-01"
#
# Searches parameters:
search_concept_gral <- "Banks"
search_concepts <- c("unicaja", "bbva", "santander")
search_places = c("ES", "ES", "ES") # ("" for all)
search_period <- "today 12-m" # Examples: "all" for all (since 1jan2004 monthly), "today+5-y" for last five years (default, weekly), "today 12-m" for 12 month from today (weekly), "today 3-m" for 3 months from today (daily), "now 7-d" for last week (hourly), "now 1-d" for last 24h (every 8 minutes), "now 4-H" for last 4h (every 5 minutes), "now 4-H" for last 60min UTC (every minute), "Y-m-d Y-m-d" for time span between two dates)
#
# Parameters OECD (leading indicators):
selected_initial_year_OECD <- "2021"
selected_end_year_OECD <-as.character(year(Sys.Date()))



# =======================================
# =======================================
# Extract weather data
allStationsData <- readRDS("./data/data_weather_ts.rds") # time-series format
# allStationsData <- readRDS("./data/data_weather_sp.rds") # spread format
# source("load_weather.R")

allData <- allStationsData

# =======================================
# =======================================
# Save to RDS
head(allData)
saveRDS(allData, "./data/data_extracted.rds")
write.csv(allData, "./data/data_extracted.csv")



