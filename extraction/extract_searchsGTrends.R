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
queryTrends$related_queries %>% filter(keyword == search_concepts[1]) %>% pull(value)
queryTrends$related_queries %>% filter(keyword == search_concepts[2]) %>% pull(value)
queryTrends$related_queries %>% filter(keyword == search_concepts[3]) %>% pull(value)

