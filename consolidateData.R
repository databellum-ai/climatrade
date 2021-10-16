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

