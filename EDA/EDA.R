
# -----------------------------------------------------------------
# -----------------------------------------------------------------
# EDA
# -----------------------------------------------------------------
# -----------------------------------------------------------------
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



