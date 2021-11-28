# rm(list = ls())

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
if(!require(tidygeocoder)) install.packages("tidygeocoder", repos = "http://cran.us.r-project.org") # Resolve coordinates of cities/places

library(tidyquant)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(tidyquant) # To get stock data
library(gtrendsR) # To get GoogleTrends data
library(ggthemes)
library(jsonlite)
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
library(tidygeocoder) # Resolve coordinates of cities/places
  
