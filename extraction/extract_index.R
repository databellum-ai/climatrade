# ================================
# Extract index data
# ================================
library(tidyverse)
library(rvest)

print("Extracting IAI index")

# https://apps.automeris.io/wpd/
historicalIAI <- readRDS("data/data_indexHistorical_ts.rds")

# Download recent data to add them to historical
# https://www.investopedia.com/anxiety-index-explained/
# OPTION 1) access to fresh data pre-downloading (and renaming) a .csv file
# recentIAI <- read.csv("data/data-IAI-fresh.csv")
# OPTION 3) dynamically access to fresh data via URL. Using data wrapping (rvest package), we dynamically obtain name of the file containing 
currentCSV <- ("https://www.investopedia.com/anxiety-index-explained/" %>% 
                 read_html() %>% html_nodes(xpath='//*[@id="mntl-sc-block-iframe__uri_1-0-1"]'))[1] %>% 
  html_attr("data-src") %>% 
  paste0("dataset.csv")
recentIAI <- read.csv(currentCSV) %>% select(Date, Value)

names(recentIAI) <- c("date", "IAI")
recentIAI <- recentIAI %>% mutate(date = as.Date(date, "%m/%d/%Y"), countryCode = NA) %>% select(date, IAI, countryCode)
IAIindex <- rbind(historicalIAI, recentIAI) %>% group_by(date,IAI,countryCode) %>% summarise(date = last(date), IAI = mean(IAI), countryCode = NA) %>% select(date, IAI, countryCode) %>% arrange(desc(date))

head(IAIindex)
IAIindex %>% arrange(desc(date))
# Save to RDS
saveRDS(IAIindex, "data/data_indexHistorical_ts.rds")

print("IAI index extraction process FINISHED")

