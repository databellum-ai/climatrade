# ================================
# Extract index data
# ================================
library(tidyverse)

print("Extracting IAI index")

# https://apps.automeris.io/wpd/
historicalIAI <- readRDS("data/data_indexHistorical_ts.rds")

# Download recent data to add them to historical
# https://www.investopedia.com/anxiety-index-explained/
# OPTION 1) access to fresh data pre-downloading (and renaming) a .csv file
# recentIAI <- read.csv("data/data-IAI-fresh.csv")
# OPTION 2) direct access to fresh data via URL
recentIAI <- read.csv("https://datawrapper.dwcdn.net/aFQir/270/dataset.csv") %>% select(Date, Value)

names(recentIAI) <- c("date", "IAI")
recentIAI <- recentIAI %>% mutate(date = as.Date(date, "%m/%d/%Y"), countryCode = NA) %>% select(date, IAI, countryCode)
IAIindex <- rbind(historicalIAI, recentIAI) %>% group_by(date,IAI,countryCode) %>% summarise(date = last(date), IAI = mean(IAI), countryCode = NA) %>% select(date, IAI, countryCode)

head(IAIindex)

# Save to RDS
saveRDS(IAIindex, "data/data_indexHistorical_ts.rds")

print("IAI index extraction process FINISHED")


