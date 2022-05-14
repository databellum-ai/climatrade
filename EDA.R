
source("initialize.R")

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
#       EXPLORATORY DATA ANALYSIS
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(openxlsx)

# --------------------------------------------------------------
# --------------------------------------------------------------
# ===============
# LOAD TRANSFORMED DATASETS, REDUCE FEATURES, ENSHORT DATE RANGE AND SAVE
# ---------------
# Define dates scope and features not necessary
initialDateAnalysis <- as_date("2017-01-01")
endDateAnalysis <- as_date("2022-04-15")
notNecessaryFeatures <- c("music.tempo_IND", "music.tempo_RUS", "music.energy_IND", "music.energy_RUS", "music.danceability_IND", "music.danceability_RUS", "OECD.BCI_IND", "OECD.BCI_RUS", "OECD.CCI_RUS", "OECD.CLI_IND", "OECD.CLI_RUS")
dataset_s1_raw <- readRDS("data/dataset_seed1_p1.rds")  # Dataset with original values customized to the existing seed and spread to final columns format
dataset_s1 <- readRDS("data/dataset_seed1_p3.rds")  # Dataset, imputated, balanced, normalized to -1000:0:1000 range
dataset_s1_001 <- dataset_s1 %>% filter(between(date, initialDateAnalysis, endDateAnalysis)) %>% select(-all_of(notNecessaryFeatures))
features <- names(dataset_s1_001)
print("All Features available:")
print(names(dataset_s1_raw))
print("All Dates available:")
print(range(dataset_s1_raw$date))
print("Features for analysis:")
print(features)
print("Dates range for analysis:")
range(dataset_s1_001$date)
# Save to .XLSX
saveRDS(dataset_s1_001,"data/dataset_s1_001.rds")  # Dataset (1000-normalized version) reduced in features and dates scope
write.xlsx(dataset_s1_001, "data/dataset_s1_001.xlsx")
# --------------------------------------------------------------
# --------------------------------------------------------------


# ===============
# GRAPHICAL ANALYSIS
# ---------------
library(ggplot2)

dataset_s1_001 <- readRDS("data/dataset_s1_001.rds")  # Load dataset for analysis
names(dataset_s1_001)







# Google searches chart
all_searches %>% group_by(KAM) %>%
  ggplot(aes(x = date, y = nSearches, color = date)) +
  geom_point(size = 1) +
  facet_wrap(~KAM) +
  ggthemes::theme_economist() + labs(title = "Interest over Time", subtitle = "Google Trends Report", caption = "By databellum®")
all_searches %>% group_by(KAM) %>%
  ggplot(aes(x = date, y = nSearches, color = KAM)) +
  geom_line(size = 1) +
  ggthemes::theme_economist() + labs(title = "Interest over Time", subtitle = "Google Trends Report", caption = "By databellum®")



# Stocks price chart
stocksData %>%
  ggplot(aes(x = date, y = value, color = symbol)) +
  geom_line() +
  facet_wrap(~symbol,scales = 'free_y') +
  theme_classic() +
  labs(x = 'Date',
       y = "Close Price",
       title = "Stocks Chart", subtitle = "", caption = "By databellum®") +
  scale_x_date(date_breaks = "month",
               date_labels = "%b\n%y") + 
  ggthemes::theme_economist()






test_data <- seedDataset3 %>% select(date, `stocks.^VIX_GLOBAL`, `stocks.^VVIX_GLOBAL`, `OECD.CLI_GER`, `index.IAI_GLOBAL`)

test_data_gathered <- test_data %>% gather(key = "variable", value = "value", -date)
ggplot(test_data_gathered, 
       aes(x = date, y = value)) + 
  geom_point(size = 0.50, aes(color = variable))

# https://epirhandbook.com/en/ggplot-basics.html
ggplot(data = test_data, 
       mapping = aes(x = `stocks.^VIX_GLOBAL`, y = `stocks.^VVIX_GLOBAL`, color = `index.IAI_GLOBAL`, size = `date`)) + 
  geom_point(alpha = 0.5, shape = "diamond")







