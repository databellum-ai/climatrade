rm(list = ls())  # clear all environment variables
graphics.off()  # clear all graphs
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(forecast)) install.packages("forecast", repos = "http://cran.us.r-project.org")
# if(!require(fpp3)) install.packages("fpp3", repos = "http://cran.us.r-project.org")
# if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(lubridate)
library(forecast)
# library(fpp3)
# library(GGally)
library(tidyquant)  # extraction from YahooFinance
library(imputeTS)  # interpolation function (na_interpolation()) during data extraction

# ===============
# ENVIRONMENT CONSTANTS
# ---------------
select <- dplyr::select # conflict between select() in MASS and in dplyr (we'll use by default)
filter <- dplyr::filter # conflict

# ------------------------------------------------------
# HYPERPARAMETERS AND CONSTANTS
frequencyNN <- 365  # daily frequency for our data (time series use year as base unit)

# ===============
# KEYS
# ---------------
# source("~/R/climatrade/keys_APIs.R")
source(getFullPath("keys_APIs.R"))

# ===============
# FUNCTION TO BUILD PATHS LOCAL/REMOTE
# Check your system information, look for nodename
print(Sys.info())
# Function to create full path
getFullPath <- function(localName) {
  if(Sys.info()["nodename"]=="ip-172-31-45-184") {
    fPath <- "~/R/climatrade/"
  } else {
    fPath <- ""
  }
  return(paste0(fPath, localName))
}






