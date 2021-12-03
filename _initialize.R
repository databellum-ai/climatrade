# ===============
# CLEAN ENVIRONMENT
# ---------------
# rm(list = ls())

# ===============
# PACKAGES REQUIRED
# ---------------
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(tidyquant)) install.packages("tidyquant", repos = "http://cran.us.r-project.org") # To get stock data
if(!require(gtrendsR)) install.packages("gtrendsR", repos = "http://cran.us.r-project.org") # To get GoogleTrends data
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org") # To get GoogleTrends data

if(!require(jsonlite)) install.packages("jsonlite", repos = "http://cran.us.r-project.org")
if(!require(spotifyr)) install.packages("spotifyr", repos = "http://cran.us.r-project.org")
if(!require(rnoaa)) install.packages("rnoaa", repos = "http://cran.us.r-project.org")
if(!require(OECD)) install.packages("OECD", repos = "http://cran.us.r-project.org")
if(!require(gtools)) install.packages("gtools", repos = "http://cran.us.r-project.org")
if(!require(tidygeocoder)) install.packages("tidygeocoder", repos = "http://cran.us.r-project.org") # Resolve coordinates of cities/places
# INSTALL SCRAPE PACKAGES:
  # 1) Ensure java installed
  if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")  # to scrape static pages
  if(!require(RSelenium)) install.packages("RSelenium", repos = "http://cran.us.r-project.org")  # to scrape dynamic  pages, includes binman
  if(!require(wdman)) install.packages("wdman", repos = "http://cran.us.r-project.org")  # for chromedriver install


# ===============
# LIBRARIES
# ---------------
library(tidyverse)

library(tidyquant)
library(lubridate)
library(tidyquant) # To get stock data
library(gtrendsR) # To get GoogleTrends data
library(ggthemes)
library(jsonlite)
library(readr)
library(data.table)
library(stringr)
library(rnoaa)
library(OECD) # To get OECD.org indicators
library(gtools)
library(tidygeocoder) # Resolve coordinates of cities/places

# Libraries Spotify (including RSelenium)
library(RSelenium)
library(wdman) # for chromedriver install (related to Rselenium)
library(binman) # for chromedriver versions check (related to Rselenium)
library(rvest)
library(spotifyr)# For certain functions and applications, you’ll need to log in as a Spotify user. To do this, your Spotify Developer application needs to have a callback url. You can set this to whatever you want that will work with your application, but a good default option is http://localhost:1410/ (see image below). For more information on authorization, visit the official Spotify Developer Guide. (https://www.rcharlie.com/spotifyr/)


# ===============
# KEYS
# ---------------
source("keys_APIs.R")
