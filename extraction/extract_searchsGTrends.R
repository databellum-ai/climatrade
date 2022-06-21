# ===============================================
# Extract search trends from Google Trends
# ===============================================
# 
# GOOGLE TRENDS. USED TOPICS CODIFICATION
# ---------------------------------------
# Macroeconomic_Anxiety:
# Inflation (/m/09jx2)
# Deflation (/m/0d126)
# +Economic anxiety (/g/11fy2vtpp2)
# +Unemployment (/m/07s_c)
# 
# Market_Anxiety:
# Short selling (/m/0sxyn)
# Volatility (/m/0ht7l)
# +VIX (/m/09fld6)
# +Bitcoin (/m/05p0rrx)
# 
# Debt&Credit_Anxiety:
# Default on debt (/m/0g1xs)
# Solvency (/g/122dlh9k)
# Bankrupcy (/m/01hhz)
# Quiebra financiera (/m/0gz_4)
# +Liquidez (/g/1q6j8vbd8)
# NOTES ON IMPLEMENTATION:
# -See README.rd for topics correspondence
# -Test responses quality depending on grpop="web" or gprop="quality"
# -Test responses quality depending on Category (Finance, News, etc.)

library(tidyverse)
library(lubridate)
library(gtrendsR)
library(ggthemes)

# ============ Load search parameters seed (Google Trends):
head(seedFeatures_df)
searchTerms <- seedFeatures_df %>% filter(source %in% c("searchesGoogle"))
tmpItems <- as.list(searchTerms$termsDetailed)
names(tmpItems) <- searchTerms$variable
searchTerms <- tmpItems
searchTerms

search_periods <- c("all", "today+5-y", "today 3-m") 
# Examples: "all" for all (since 1jan2004 monthly), "today+5-y" for last five years (default, weekly), "today 12-m" for 12 month from today (weekly), "today 3-m" for 3 months from today (daily), "now 7-d" for last week (hourly), "now 1-d" for last 24h (every 8 minutes), "now 4-H" for last 4h (every 5 minutes), "now 4-H" for last 60min UTC (every minute), "Y-m-d Y-m-d" for time span between two dates)

all_searches <- data.frame()
print("Getting searches")
for (i in c(1:length(searchTerms))) { # loop "vectors of terms" (KAM) within main list
  search_KAM <- names(searchTerms)[i]
  search_term <- searchTerms[[i]]
  search_term <- unlist(str_split(search_term,", ")) # convert to vector
  if (search_term[1]=="") {   # assume concept term (KAM) if no detailed list of terms supplied
    search_term <- search_KAM
  }
  if (length(search_term) > 5) {   # limit to 5 terms
    search_term <- search_term[1:5]
  }
  print(paste(search_KAM,"::",search_term))
  for (j in c(1:length(search_periods))) { # loop resolutions to cover all possible values
    print(paste("...",search_periods[j]))
    # Connect with Google to obtain a list of dataframes
    queryTrends <- gtrends(
      gprop = "news",
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


# ===============================================
# RESOLUTIONS BALANCING
# ===============================================
# We are mixing data from different resolutions to optimize full picture granularity. We'll balance the higher resolutions based of the full picture weight for that specific period, so percentages are more consistent

print("Processing normalization")

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
# The general one-line formula to linearly rescale data values having observed min and max into a new arbitrary range min' to max' is
# newvalue= (max'-min')/(max-min)*(value-min)+min'
all_searches <- all_searches %>%
  left_join(tmpRangesLowResol) %>% 
  mutate(normHits = (high - low) / (100 - 0.5) * (hits - low) + low) %>% 
  mutate(normHits = ifelse(is.na(normHits), hits, normHits)) %>% 
  select(date, KAM, keyword, time, hits, normHits, low, high)

head(all_searches)

# ===============================================
# PREPARE OUTPUTS
# ===============================================
# group by date and KAM
all_searches <- all_searches %>% 
  mutate(date =  as.Date(date)) %>% 
  group_by(date, KAM) %>% 
  summarise(nSearches = mean(normHits))
head(all_searches)

# spread
all_searches_ts <- all_searches %>% 
  spread(key=KAM, value=nSearches) %>% mutate(countryCode = NA)
head(all_searches_ts)

# ===============================================
# SAVE RESULTS
# ===============================================
saveRDS(as_tibble(all_searches_ts), "data/data_searchesGoogle_ts.rds")

print("End of search categories extraction process")













