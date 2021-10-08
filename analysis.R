
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(tidyquant)) install.packages("tidyquant", repos = "http://cran.us.r-project.org") # To get sotck data
if(!require(gtrendsR)) install.packages("gtrendsR", repos = "http://cran.us.r-project.org") # To get GoogleTrends data
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org") # To show correlograms

library(tidyverse)
library(ggplot2)
library(tidyquant) # To get sotck data
library(gtrendsR) # To get GoogleTrends data
library(corrplot) # To show correlograms


# ================================
# Parameters
# ================================
# Prices parameters:
chosenTickers = c("AAPL", "NFLX", "AMZN", "AAIC", "MSFT", "AAN")
endDateTicker = today
startingDateTicker = endDateTicker - 365 # also as... "2021-01-01"
# Searches parameters:
search_concept_gral <- "Banks"
search_concepts <- c("unicaja", "bbva", "santander")
search_places = c("ES", "ES", "ES") # ("" for all)
search_period <- "today 12-m" # Examples: "all" for all (since 1jan2004 monthly), "today+5-y" for last five years (default, weekly), "today 12-m" for 12 month from today (weekly), "today 3-m" for 3 months from today (daily), "now 7-d" for last week (hourly), "now 1-d" for last 24h (every 8 minutes), "now 4-H" for last 4h (every 5 minutes), "now 4-H" for last 60min UTC (every minute), "Y-m-d Y-m-d" for time span between two dates)



# ================================
# Extract stock data
# ================================
prices <- tq_get(chosenTickers,
                 from = startingDateTicker,
                 to = endDateTicker,
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
  ggthemes::theme_economist() + labs(title = paste("Interest over Time - aggregating concept: ", search_concept_gral), subtitle = "Based on Google search terms", caption = "By databellum®")

# Extract dataframe containing related searches
searches_related <- queryTrends %>% .$related_queries
head(searches_related)
queryTrends$related_queries %>% filter(keyword == search_concept[1]) %>% pull(value)
queryTrends$related_queries %>% filter(keyword == search_concept[2]) %>% pull(value)
queryTrends$related_queries %>% filter(keyword == search_concept[3]) %>% pull(value)

# ===============================================
# Consolidation
# Data obtained (output dataframes)
# ===============================================
head(prices)
head(searches_iot) # Hits over time for the concepts requested
head(searches_aggrHits) # Hits over time aggregating concepts
head(searches_related) # Search terms related with concepts requested



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

'-------------------------------------'
# https://www.lobdata.com.br/2020/09/15/how-to-perform-correlation-analysis-in-time-series-data-using-r/
# https://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html

head(indicators)

# packages for this tutorial
install.packages("feasts")
install.packages("tsibble")
install.packages("lubridate")

# second approach
install.packages("TSstudio")
install.packages("plotly")

# third approach
install.packages("timetk")
install.packages("lubridate")

# first approach
library(feasts)
library(tsibble)
library(lubridate)

# second approach
library(TSstudio)
library(plotly)

# third approach
library(tidyverse)
library(timetk)
library(lubridate)


indicators %>% select(-source) %>% spread(indicator, rawValue)

