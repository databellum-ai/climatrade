extractDataUptodate <- function() {
  # PlanetMood dataset
  currentData <- readRDS("data/df_planetMood.rds") %>% arrange(date) %>% mutate(VIX = VIX+30) # Dataset ready for analysis 
  selectedVbles <- c("VIX", "VVIX", "VIX3M", "VIXNsdq", "GoldVlty", "DAI3", "CCI")
  currentData <- currentData %>% 
    select(date, 
           selectedVbles, 
           MoonPhase, WkDay, YrWeek)
  return(currentData)
}

