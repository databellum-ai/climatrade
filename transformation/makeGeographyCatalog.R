library(tidyverse)
library(openxlsx)

std_geo_revised <- read.xlsx("userEdition/standardGeography.xlsx")


std_geo_revised %>% select(-c(countryCode, countryName))

