# PTE: criterio de agrupación con los varios periodos de resolución: OJO conversión de porcentajes(!)

# ===============================================
# Extract search trends from Google Trends
# ===============================================
# https://www.rdocumentation.org/packages/gtrendsR/versions/1.3.5/topics/gtrends

library(gtrendsR)
library(ggthemes)

search_periods <- c("all", "today+5-y", "today 3-m", "now 7-d") 
# Examples: "all" for all (since 1jan2004 monthly), "today+5-y" for last five years (default, weekly), "today 12-m" for 12 month from today (weekly), "today 3-m" for 3 months from today (daily), "now 7-d" for last week (hourly), "now 1-d" for last 24h (every 8 minutes), "now 4-H" for last 4h (every 5 minutes), "now 4-H" for last 60min UTC (every minute), "Y-m-d Y-m-d" for time span between two dates)

all_searches <- data.frame()
# >>>>>
# >>>>>
for (i in c(1:length(searchTerms))) { # loop "vectors of terms" (KAM) within main list
  search_KAM <- names(searchTerms)[i]
  search_terms <- searchTerms[[i]] 
  print(paste("Getting searches of:",search_KAM,"::",search_terms))
  # >>>>>
  # >>>>>
  for (j in c(1:length(search_periods))) { # loop resolutions to cover all possible values
    # Connect with Google to obtain a list of dataframes
    queryTrends <- gtrends(
      keyword = search_terms, 
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
  # <<<<<
  # <<<<<
}
# <<<<<
# <<<<<
head(all_searches)

# prepare to group and spread
all_searches_ts <- all_searches %>% 
  mutate(date =  as.Date(date)) %>% 
  group_by(date, KAM) %>% summarise(hits = mean(hits)) %>% spread(key=KAM, value=hits)
head(all_searches_ts)


# ===============================================
# SAVE RESULTS
# ===============================================
saveRDS(all_searches_ts, "data/data_searchesGoogle_ts.rds")

# ----------
# CHARTS
# Chart of interest over time for each search "concept"
all_searches %>% 
  ggplot(aes(x = date, y = hits)) +
  geom_line(colour = "darkblue", size = 1.5) +
  facet_wrap(~keyword) +
  ggthemes::theme_economist() + labs(title = "Interest over Time", subtitle = "Google Trends Report", caption = "By databellum®")




