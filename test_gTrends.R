# ===============================================
# Extract search trends from Googles Trends
# ===============================================

if(!require(gtrendsR)) install.packages("gtrendsR", repos = "http://cran.us.r-project.org")
library(gtrendsR)
library(tidyverse)
library(ggplot2)

# Parameters
search_terms <- c("unicaja", "bbva", "santander")
places = c("ES", "ES", "ES") # ("" for all)
period <- "today 3-m" # Examples: "all" for all (since 1jan2004 monthly), "today+5-y" for last five years (default, weekly), "today 12-m" for 12 month from today (weekly), "today 3-m" for 3 months from today (daily), "now 7-d" for last week (hourly), "now 1-d" for last 24h (every 8 minutes), "now 4-H" for last 4h (every 5 minutes), "now 4-H" for last 60min UTC (every minute), "Y-m-d Y-m-d" for time span between two dates)
restrictToInterest = FALSE # only interest-over-time is faster
# Connect with Google
queryTrends <- gtrends(keyword = search_terms, geo = places, time = period, onlyInterest = restrictToInterest)
class(queryTrends)
summary(queryTrends)

# Extract dataframe containing hits over time
searches_iot <- queryTrends %>% .$interest_over_time %>%
  mutate_at("hits", ~ifelse(. == "<1", 0.5, .)) %>% # replace with 0.5 instead of a character
  mutate_at("hits", ~as.numeric(.)) # convert to numeric
# View
class(searches_iot)
head(searches_iot)
# Chart of interest over time
searches_iot %>% 
  ggplot(aes(x = date, y = hits)) +
  geom_line(colour = "darkblue", size = 1.5) +
  facet_wrap(~keyword) +
  ggthemes::theme_economist() + labs(title = "Interest over Time", subtitle = "Google Trends Report", caption = "By databellum®")

# Extract dataframe containing related searches
searches_related <- queryTrends %>% .$related_queries
# View
class(searches_related)
head(searches_related)
queryTrends$related_queries %>% filter(keyword == search_terms[1]) %>% pull(value)
queryTrends$related_queries %>% filter(keyword == search_terms[2]) %>% pull(value)
queryTrends$related_queries %>% filter(keyword == search_terms[3]) %>% pull(value)



