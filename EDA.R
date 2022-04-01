
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

# -----------------------------------------------------------------
# -----------------------------------------------------------------
# EDA
# ANALYSIS 001
# 2017-01-01 to 2022-03-20
# -----------------------------------------------------------------
# -----------------------------------------------------------------

# ===============
# LOAD TRANSFORMED DATASETS
# ---------------
dataset_s1_raw <- readRDS("data/dataset_seed1_p1.rds")  # Dataset with original values customized to the existing seed and spread to final columns format
dataset_s1 <- readRDS("data/dataset_seed1_p3.rds")  # Dataset, imputated, balanced, normalized to -1000:0:1000 range
# ===============
# DEFINE DATES SCOPE AND REMOVE NOT NECESSARY FEATURES
# ---------------
initialDateAnalysis <- as_date("2017-01-01")
endDateAnalysis <- as_date("2022-02-15")
notNecessaryFeatures <- c("music.tempo_IND", "music.tempo_RUS", "music.energy_IND", "music.energy_RUS", "music.danceability_IND", "music.danceability_RUS", "OECD.BCI_IND", "OECD.BCI_RUS", "OECD.CCI_RUS", "OECD.CLI_IND", "OECD.CLI_RUS")
dataset_s1_001 <- dataset_s1 %>% filter(between(date, initialDateAnalysis, endDateAnalysis)) %>% select(-all_of(notNecessaryFeatures))
range(dataset_s1_001$date)
features <- names(dataset_s1_001)
features


write.xlsx(dataset_s1_001, "data/dataset_s1_001.xlsx")






# ===============
# GRAPHICAL ANALYSIS
# ---------------
library(ggplot2)

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






test_data <- seedDataset3 %>% select(date, `stocks.^VIX_GLOBAL`, `stocks.^VVIX_GLOBAL`, `OECD.CLI_OECD`, `index.IAI_GLOBAL`)

test_data_gathered <- test_data %>% gather(key = "variable", value = "value", -date)
ggplot(test_data_gathered, 
       aes(x = date, y = value)) + 
  geom_point(size = 0.50, aes(color = variable))

# https://epirhandbook.com/en/ggplot-basics.html
ggplot(data = test_data, 
       mapping = aes(x = `stocks.^VIX_GLOBAL`, y = `stocks.^VVIX_GLOBAL`, color = `index.IAI_GLOBAL`, size = `date`)) + 
  geom_point(alpha = 0.5, shape = "diamond")







