# PlanetMood dataset
df_planetMood <- readRDS("data/df_planetMood.rds") %>% arrange(date) %>% mutate(VIX = VIX+30) # Dataset ready for analysis 
names(df_planetMood)
df_planetMood <- df_planetMood %>% 
  cbind(dateQuartile) %>% 
  mutate(Year = year(date)) %>% 
  select(date, 
         selectedVbles, 
         MoonPhase, WkDay, YrWeek, Year)