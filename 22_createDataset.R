
source("10_initialize.R")

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# LOAD TRANSFORMED DATASETS, REDUCE FEATURES, ENSHORT DATE RANGE AND SAVE
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(openxlsx)

dataset_s1_raw <- readRDS("data/dataset_seed1_p1.rds")  # Dataset with original values customized to the existing seed and spread to final columns format
df_planetMood <- readRDS("data/dataset_seed1_p2.rds")  # Dataset, imputated, balanced
# df_planetMood <- readRDS("data/dataset_seed1_p3.rds")  # Dataset, imputated, balanced, normalized to -1000:0:1000 range

# Define dates scope and features not necessary
initialDateAnalysis <- as_date("2017-01-01")
endDateAnalysis <- as_date("2022-04-15")
notNecessaryFeatures <- c(
  "music.tempo_IND", 
  "music.tempo_RUS", 
  "music.energy_IND", 
  "music.energy_RUS", 
  "music.danceability_IND", 
  "music.danceability_RUS", 
  "OECD.BCI_IND", 
  "OECD.BCI_RUS", 
  "OECD.CCI_RUS", 
  "OECD.CLI_IND", 
  "OECD.CLI_RUS"
  )
df_planetMood <- df_planetMood %>% filter(between(date, initialDateAnalysis, endDateAnalysis)) %>% select(-all_of(notNecessaryFeatures))
print(paste("ALL DATES RANGE AVAILABLE:", paste(shQuote(range(dataset_s1_raw$date), type="sh"), collapse=" to ")))
print(paste("ALL FEATURES AVAILABLE:", paste(shQuote(names(dataset_s1_raw), type="sh"), collapse=", ")))
print("-----")
print(paste("DATES RANGE FOR ANALYSIS:", paste(shQuote(range(df_planetMood$date), type="sh"), collapse=" to ")))
print(paste("FEATURES FOR ANALYSIS:", paste(shQuote(names(df_planetMood), type="sh"), collapse=", ")))

# Save to .XLSX
saveRDS(df_planetMood,"data/df_planetMood.rds")  # Dataset (1000-normalized version) reduced in features and dates scope
write.xlsx(df_planetMood, "data/df_planetMood.xlsx")


