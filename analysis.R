
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(tidyquant)) install.packages("tidyquant", repos = "http://cran.us.r-project.org") # To get sotck data
if(!require(gtrendsR)) install.packages("gtrendsR", repos = "http://cran.us.r-project.org") # To get GoogleTrends data

library(tidyverse)
library(ggplot2)
library(tidyquant) # To get sotck data
library(gtrendsR) # To get GoogleTrends data

# ================================
# Parameters
# ================================
# Prices parameters:
chosenTickers = c("AAPL", "NFLX", "AMZN", "AAIC", "MSFT", "AAN")
# Searches parameters:
search_concepts <- c("unicaja", "bbva", "santander")
search_places = c("ES", "ES", "ES") # ("" for all)
search_period <- "today 3-m" # Examples: "all" for all (since 1jan2004 monthly), "today+5-y" for last five years (default, weekly), "today 12-m" for 12 month from today (weekly), "today 3-m" for 3 months from today (daily), "now 7-d" for last week (hourly), "now 1-d" for last 24h (every 8 minutes), "now 4-H" for last 4h (every 5 minutes), "now 4-H" for last 60min UTC (every minute), "Y-m-d Y-m-d" for time span between two dates)

# ================================
# Retrieve stock data
# ================================



prices <- tq_get(chosenTickers,
                 from = "2021-01-01",
                 to = "2021-10-01",
                 get = "stock.prices")

head(prices)

prices %>%
  ggplot(aes(x = date, y = adjusted, color = symbol)) +
  geom_line() +
  facet_wrap(~symbol,scales = 'free_y') +
  theme_classic() +
  labs(x = 'Date',
       y = "Adjusted Price",
       title = "Price Chart", subtitle = "", caption = "By databellum®") +
  scale_x_date(date_breaks = "month",
               date_labels = "%b\n%y") + 
  ggthemes::theme_economist()


# ===============================================
# Extract search trends from Googles Trends
# ===============================================
restrictToInterest = FALSE # only interest-over-time is faster

# Connect with Google to obtain a list of dataframes
queryTrends <- gtrends(keyword = search_concepts, geo = search_places, time = search_period, onlyInterest = restrictToInterest)

# Extract dataframe containing hits over time
searches_iot <- queryTrends %>% .$interest_over_time %>%
  mutate_at("hits", ~ifelse(. == "<1", 0.5, .)) %>% # replace with 0.5 instead of a character
  mutate_at("hits", ~as.numeric(.)) # convert to numeric
# Chart of interest over time for each search "concept"
searches_iot %>% 
  ggplot(aes(x = date, y = hits)) +
  geom_line(colour = "darkblue", size = 1.5) +
  facet_wrap(~keyword) +
  ggthemes::theme_economist() + labs(title = "Interest over Time", subtitle = "Google Trends Report", caption = "By databellum®")

# A consolidation of hits of all concepts (still normalized to 100)
searches_aggrHits <- searches_iot %>% 
  group_by(date) %>% 
  summarize(hits_aggreg = sum(hits)) %>% mutate(hits_aggreg = round(100 * hits_aggreg / max(.$hits_aggreg),0))
# Chart of interest over time for all search concepts normalized to 100
searches_aggrHits %>% 
  ggplot(aes(x = date, y = hits_aggreg)) +
  geom_line(colour = "darkblue", size = 1.5) +
  ggthemes::theme_economist() + labs(title = "Interest over Time - aggregating concepts", subtitle = "Google Trends Report", caption = "By databellum®")

# Extract dataframe containing related searches
searches_related <- queryTrends %>% .$related_queries
head(searches_related)
queryTrends$related_queries %>% filter(keyword == search_concept[1]) %>% pull(value)
queryTrends$related_queries %>% filter(keyword == search_concept[2]) %>% pull(value)
queryTrends$related_queries %>% filter(keyword == search_concept[3]) %>% pull(value)

# Data obtained (output dataframes)
head(searches_iot) # Hits over time for the concepts requested
head(searches_aggrHits) # Hits over time aggregating concepts
head(searches_related) # Search terms related with concepts requested

