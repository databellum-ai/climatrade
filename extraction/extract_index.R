# Imputación de huecos día a día en historicalIAI2: del 2021-01-23 al 2021-03-08
# Leer .csv actual y renombrar 

# ================================
# Extract index data
# ================================
library(tidyverse)

print("Extracting IAI index")

# https://apps.automeris.io/wpd/
historicalIAI <- readRDS("data/data_indexHistorical_ts.rds")

# https://www.investopedia.com/anxiety-index-explained/
recentIAI <- read.csv("data/data-IAI-fresh.csv")
names(recentIAI) <- c("date", "IAI")
recentIAI <- recentIAI %>% mutate(date = as.Date(date, "%m/%d/%Y"), countryCode = NA) %>% select(date, IAI, countryCode)
IAIindex <- rbind(historicalIAI, recentIAI) %>% group_by(date,IAI,countryCode) %>% summarise(date = last(date), IAI = mean(IAI), countryCode = NA) %>% select(date, IAI, countryCode)

head(IAIindex)

# Save to RDS
saveRDS(IAIindex, "data/data_indexHistorical_ts.rds")

print("IAI index extraction process FINISHED")

library(openxlsx)
write.xlsx(IAIindex, "data/test1.xlsx")
