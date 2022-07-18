
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

# Define dates scope and features not necessary
initialDateAnalysis <- as_date("2017-01-01")
endDateAnalysis <- Sys.Date()-2 # Two day to allow data get updated from sources

dataset_s1_raw <- readRDS("data/dataset_seed1_p1.rds")  # Dataset with original values customized to the existing seed and spread to final columns format
df_planetMood <- readRDS("data/dataset_seed1_p2.rds")  # Dataset, imputated, balanced
# df_planetMood <- readRDS("data/dataset_seed1_p3.rds")  # Dataset, imputated, balanced, normalized to -1000:0:1000 range
df_planetMood <- df_planetMood %>% 
  filter(between(date, initialDateAnalysis, endDateAnalysis)) %>% arrange(desc(date)) %>% 
  select(
    date, 
    "VIX" = "stocks.^VIX_GLOBAL", 
    "Gold" = "stocks.GC=F_GLOBAL", 
    "SP500" = "stocks.^GSPC_GLOBAL", 
    "VVIX" = "stocks.^VVIX_GLOBAL", 
    "VIX3M" = "stocks.^VIX3M_GLOBAL", 
    "VIXNsdq" = "stocks.^VXN_GLOBAL", 
    "GoldVlty" = "stocks.^GVZ_GLOBAL", 
    "VIX_HLvol" = "stocks.^VIXhlVOL_GLOBAL", 
    "VVIX_HLvol" = "stocks.^VVIXhlVOL_GLOBAL", 
    "VIX3M_HLvol" = "stocks.^VIX3MhlVOL_GLOBAL", 
    "VIXNsdq_HLvol" = "stocks.^VXNhlVOL_GLOBAL", 
    "GoldVlty_HLvol" = "stocks.^GVZhlVOL_GLOBAL", 
    "Gold_HLvol" = "stocks.GC=FhlVOL_GLOBAL", 
    "SP500_HLvol" = "stocks.^GSPChlVOL_GLOBAL", 
    "NewsTone" = "GDELT.tone_GLOBAL", 
    "Goldstein" = "GDELT.goldstein_GLOBAL",     
    "IAI" = "index.IAI_GLOBAL" , 
    "DAI1" = "searchesGoogle.DAI1_GLOBAL", 
    "DAI2" = "searchesGoogle.DAI2_GLOBAL", 
    "DAI3" = "searchesGoogle.DAI3_GLOBAL",      
    "BCI" = "OECD.BCI_OECD", 
    "CCI"= "OECD.CCI_OECD", 
    "CLI" = "OECD.CLI_OECD",   
    "Flights" = "airTraffic.worldFlights_GLOBAL", 
    "Tempo" = "music.tempo_GLOBAL", 
    "Energy" = "music.energy_GLOBAL", 
    "Danceability" = "music.danceability_GLOBAL", 
    "MoonPhase" = "moonSunCalendar.moonPhase_GLOBAL", 
    "WkDay" = "moonSunCalendar.weekday_GLOBAL", 
    "YrWeek" = "moonSunCalendar.yearWeek_GLOBAL"
  )

print(paste("ALL DATES RANGE AVAILABLE:", paste(shQuote(range(dataset_s1_raw$date), type="sh"), collapse=" to ")))
print(paste("ALL FEATURES AVAILABLE:", paste(shQuote(names(dataset_s1_raw), type="sh"), collapse=", ")))
print("-----")
print(paste("DATES RANGE FOR ANALYSIS:", paste(shQuote(range(df_planetMood$date), type="sh"), collapse=" to ")))
print(paste("FEATURES FOR ANALYSIS:", paste(shQuote(names(df_planetMood), type="sh"), collapse=", ")))

# Save to .XLSX
saveRDS(df_planetMood,"data/df_planetMood.rds")  # Dataset (imputated version) reduced in features and dates scope
write.xlsx(df_planetMood, "data/df_planetMood.xlsx")


