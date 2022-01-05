# -----------------------------------------------------------------
# -----------------------------------------------------------------
# Data transformation from .RDS extracted files
# -----------------------------------------------------------------
# -----------------------------------------------------------------


# =========================================
# Transform weather from NOAA
# =========================================
allStationsData_ts <- readRDS("data/data_weather_ts.rds") # time-series format
allStationsData_sp <- readRDS("data/data_weather_sp.rds") # spread format


# =========================================
# Transform Moon angle data
# =========================================


# =========================================
# Transform Twitter posts related data
# =========================================


# =========================================
# Transform from news
# =========================================


# =========================================
# Transform from FIFA Ranking
# =========================================


# =========================================
# Transform stock prices from Yahoo Finance
# =========================================


# =========================================
# Transform searches from Google Trends
# =========================================



# =========================================
# Extract leading indicators from OECD
# =========================================



# =========================================
# Extract music downloads from SPOTIFY
# =========================================


# =======================================
# Save to RDS
# =======================================
# allData <- rbind(..., ..., ...)
# head(allData)
# saveRDS(allData, "data/data_extracted.rds")
# write.csv(allData, "data/data_extracted.csv")



# ===============================================
# Consolidation
# Data obtained (output dataframes)
# ===============================================
head(prices)
head(searches_iot) # Hits over time for the concepts requested
head(searches_aggrHits) # Hits over time aggregating concepts
head(searches_related) # Search terms related with concepts requested
head(historicalRankingFIFA) # FIFA historical ranking



indicators <- prices %>% 
  mutate(indicator = symbol, rawValue = close, source = "ClosePrice") %>% select(date, indicator, rawValue, source) %>% 
  rbind(
    searches_aggrHits %>% 
      mutate(indicator = search_concept_gral, rawValue = hits_aggreg, source = "Searches") %>% select(date, indicator, rawValue, source))
unique(indicators$indicator)

indicators %>%
  ggplot(aes(x = date, y = rawValue, color = indicator)) +
  geom_line() +
  facet_wrap(~indicator,scales = 'free_y') +
  theme_classic() +
  labs(x = 'Date',
       y = "Raw Value",
       title = "Indicators Chart", subtitle = "", caption = "By databellum®") +
  scale_x_date(date_breaks = "month",
               date_labels = "%b\n%y") + 
  ggthemes::theme_economist()
