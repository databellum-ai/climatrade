# -- FIFA es un ranking: 1) su evolución no es significativa + 2) estaría invertido (mejor es 1 y sólo hay ~200 países)
# -- MUSICA/OECD/FIFA: ¿debe compararse sólo consigo mismos o también respecto al GLOBAL/MEDIANA?
# -- Reducir scope temporal: "2002-01-01"?


# -----------------------------------------------------------------
# -----------------------------------------------------------------
# EDA
# -----------------------------------------------------------------
# -----------------------------------------------------------------

library(tidyverse)
library(openxlsx)

# ------------------------------------------------------
# Load transformed datasets
# ------------------------------------------------------
seedDataset <- readRDS("data/dataset_seed1_p1.rds")  # Dataset with original values customized to the existing seed and spread to final columns format
seedDataset2 <- readRDS("data/dataset_seed1_p2.rds")  # Dataset including imputation of missing values and removing empty columns
seedDataset3 <- readRDS("data/dataset_seed1_p3.rds")  # Dataset adding conversion to 1:1000 range







# Google searches chart
all_searches %>% group_by(KAM) %>%
  ggplot(aes(x = date, y = nSearches, color = date)) +
  geom_point(size = 1) +
  facet_wrap(~KAM) +
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
# Stocks volume chart
stocksVolume %>%
  ggplot(aes(x = date, y = value, color = symbol)) +
  geom_line() +
  facet_wrap(~symbol,scales = 'free_y') +
  theme_classic() +
  labs(x = 'Date',
       y = "Volume",
       title = "Stocks", subtitle = "", caption = "By databellum®") +
  scale_x_date(date_breaks = "month",
               date_labels = "%b\n%y") + 
  ggthemes::theme_economist()


# OECD chart
leadingIndicatorsOECD %>%
  ggplot(aes(as_date(Date), OECD_CLI, color=Country)) + geom_line()



