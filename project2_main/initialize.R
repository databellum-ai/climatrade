rm(list = ls())  # clear all environment variables
graphics.off()  # clear all graphs
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(forecast)) install.packages("forecast", repos = "http://cran.us.r-project.org")
if(!require(fpp3)) install.packages("fpp3", repos = "http://cran.us.r-project.org")
# if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(forecast)
library(fpp3)
# library(GGally)

# ------------------------------------------------------
# HYPERPARAMETERS AND CONSTANTS
# regressors_set <- "VX+C1" # ("VIX_n", "VVIX_n", "VIX3M_n", "VIXNsdq_n", "GoldVlty_n")
frequencyNN <- 365  # daily frequency for our data (time series use year as base unit)

# ------------------------------------------------------
# INCLUDED FUNCTIONS
source("project2_main/extractDataUptodate.R")  # function to generate recommendations using NN forecast
source("project2_main/generateRecommendations.R")  # function to generate recommendations using NN forecast