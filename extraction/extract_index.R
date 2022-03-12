# ================================
# Extract index data
# ================================
library(tidyverse)

print("Extracting IAI index")

historicalIAI <- readRDS("data/data_index_ts.rds")

recentIAI <- read.csv("data/data-aFQir.csv")
names(recentIAI) <- c("date", "IAI")
recentIAI <- recentIAI %>% mutate(date = as.Date(date, "%m/%d/%Y"), countryCode = NA) %>% select(date, IAI, countryCode)
IAIindex <- rbind(historicalIAI, recentIAI) %>% group_by(date,IAI,countryCode) %>% summarise(date = last(date), IAI = mean(IAI), countryCode = NA) %>% select(date, IAI, countryCode)

head(IAIindex)

# Save to RDS
saveRDS(IAIindex, "data/data_index_ts.rds")

print("IAI index extraction process FINISHED")

