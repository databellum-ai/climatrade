
source("initialize.R")

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
#       EXPLORATORY DATA ANALYSIS
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

# PTE: MUSICA/OECD: ¿debe compararse sólo consigo mismos o también respecto al GLOBAL/MEDIANA?
# PTE: Reducir scope temporal: "2002-01-01"?
# PTE: decidir cómo incorporaré features categóricas (p.e. weekday) que no están en la seed

library(tidyverse)
library(openxlsx)

# -----------------------------------------------------------------
# -----------------------------------------------------------------
# EDA
# -----------------------------------------------------------------
# -----------------------------------------------------------------
# ===============
# LOAD TRANSFORMED DATASETS
# ---------------
seedDataset <- readRDS("data/dataset_seed1_p1.rds")  # Dataset with original values customized to the existing seed and spread to final columns format
seedDataset2 <- readRDS("data/dataset_seed1_p2.rds")  # Dataset including imputation of missing values and removing empty columns
seedDataset3 <- readRDS("data/dataset_seed1_p3.rds")  # Dataset adding conversion to 1:1000 range
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






test_data <- seedDataset3 %>% select(date, `stocks.USDGBP=X_GLOBAL`, `stocks.^N225_GLOBAL`, `OECD.CLI_OECD`)

test_data_gathered <- test_data %>% gather(key = "variable", value = "value", -date)
ggplot(test_data_gathered, 
       aes(x = date, y = value)) + 
  geom_line(size = 0.50, aes(color = variable))

# https://epirhandbook.com/en/ggplot-basics.html
ggplot(data = test_data, 
       mapping = aes(x = `stocks.USDGBP=X_GLOBAL`, y = `stocks.^N225_GLOBAL`, color = `date`, size = `OECD.CLI_OECD`)) + 
  geom_point(alpha = 0.5, shape = "diamond")







