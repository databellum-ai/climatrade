# PTE: criterio de agrupación con los varios periodos de resolución: OJO conversión de porcentajes(!)

# ===============================================
# Extract search trends from Google Trends
# ===============================================
# https://www.rdocumentation.org/packages/gtrendsR/versions/1.3.5/topics/gtrends

library(tidyverse)
library(lubridate)
library(gtrendsR)
library(ggthemes)

search_periods <- c("all", "today+5-y", "today 3-m", "now 7-d") 
# Examples: "all" for all (since 1jan2004 monthly), "today+5-y" for last five years (default, weekly), "today 12-m" for 12 month from today (weekly), "today 3-m" for 3 months from today (daily), "now 7-d" for last week (hourly), "now 1-d" for last 24h (every 8 minutes), "now 4-H" for last 4h (every 5 minutes), "now 4-H" for last 60min UTC (every minute), "Y-m-d Y-m-d" for time span between two dates)

all_searches <- data.frame()
print("Getting searches")
for (i in c(1:length(searchTerms))) { # loop "vectors of terms" (KAM) within main list
  search_KAM <- names(searchTerms)[i]
  search_term <- searchTerms[[i]]
  print(paste(search_KAM,"::",search_term))
  for (j in c(1:length(search_periods))) { # loop resolutions to cover all possible values
    print(paste("...",search_periods[j]))
    # Connect with Google to obtain a list of dataframes
    queryTrends <- gtrends(
      keyword = search_term, 
      time = search_periods[j], 
      onlyInterest = TRUE) # only interest-over-time is faster
    # Extract dataframe containing hits over time
    searches_iot <- queryTrends %>% .$interest_over_time %>%
      mutate_at("hits", ~ifelse(. == "<1", 0.5, .)) %>% # replace with 0.5 instead of a character
      mutate_at("hits", ~as.numeric(.)) %>%  # convert to numeric
      mutate(KAM = search_KAM) %>%
      select(date, KAM, keyword, hits, time)
    all_searches <- rbind(all_searches, searches_iot)
  }  
}

head(all_searches)

# ======================
# Resolutions balancing
# We are mixing data from different resolutions to optimize full picture granularity. We'll balance the higher resolutions based of the full picture weight for that specific period, so percentages are more consistent
tmpRanges_3m <- all_searches %>% 
  filter(time == "all" & (date >= as.Date(Sys.time()) %m-% months(3))) %>% 
  group_by(KAM) %>% 
  summarise(low=min(hits), high=max(hits)) %>% 
  mutate(time="today 3-m") %>% relocate(time)
tmpRanges_5y <- all_searches %>% 
  filter(time == "all" & (date >= as.Date(Sys.time()) %m-% years(5))) %>% 
  group_by(KAM) %>% 
  summarise(low=min(hits), high=max(hits)) %>% 
  mutate(time="today+5-y") %>% relocate(time)
tmpRangesLowResol <- rbind(tmpRanges_3m, tmpRanges_5y)
tmpRangesHighResol <- all_searches %>% 
  filter(time %in% c("today+5-y", "today 3-m")) %>% 
  group_by(time, KAM) %>% 
  summarise(low=min(hits), high=max(hits))
tmpRangesLowResol
tmpRangesHighResol





# lowResolLast3months_mean <- 
#   all_searches %>% filter(time == "all" & date >= (as.Date(Sys.time()) %m-% months(3))) %>% pull(hits) %>% mean()
# higherResolLast3months_mean <- 
#   all_searches %>% filter(time == "today 3-m") %>%  pull(hits) %>% mean()
# lowResolLast5years_mean <- 
#   all_searches %>% filter(time == "all" & date >= (as.Date(Sys.time()) %m-% years(5))) %>% pull(hits) %>% mean()
# higherResolLast5years_mean <- 
#   all_searches %>% filter(time == "today+5-y") %>% pull(hits) %>% mean()
# diffResolsMean3months <- higherResolLast3months_mean - lowResolLast3months_mean
# diffResolsMean5years <- higherResolLast5years_mean - lowResolLast5years_mean
# diffResolsMean3months
# diffResolsMean5years
# # Now we just correct relative, recent data to weight them according to full picture for those periods
# all_searches <- all_searches %>% 
#   mutate(hits = case_when(
#     time=="now 7-d" ~ (hits),
#     time=="today 3-m" ~ (hits - diffResolsMean3months), 
#     time=="today+5-y" ~ (hits - diffResolsMean5years),
#     time=="all" ~ (hits)))
# # The problem is each resolution is obtained ranges [0:100], but after balancing, the full range expands from <0 to >100. We want to keep [0:100], so we'll scale
# max(all_searches$hits)
# min(all_searches$hits)






all_searches <- all_searches %>% 
  mutate(date =  as.Date(date)) %>% 
  group_by(date, KAM) %>% 
  summarise(hits = mean(hits))
head(all_searches_ts)

# spread
all_searches_ts <- all_searches %>% 
  spread(key=KAM, value=hits)
head(all_searches_ts)
# ===============================================
# SAVE RESULTS
# ===============================================
saveRDS(all_searches_ts, "data/data_searchesGoogle_ts.rds")




