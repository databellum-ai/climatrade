#========================================================
# Function to extract daily data since beginning until last close
extractDataUptodate <- function() {
  df_planetMood <- readRDS("data/df_planetMood.rds") %>% arrange(date) %>% 
    mutate(VIX = VIX+30, Year = year(date)) # Dataset ready for analysis 
  allVbles <- names(df_planetMood)
  # allVbles <- c("VIX", "Gold", "SP500", "VVIX", "VIX3M", "VIXNsdq", "GoldVlty", "VIX_HLvol", "VVIX_HLvol", "VIX3M_HLvol", "VIXNsdq_HLvol", "GoldVlty_HLvol", "Gold_HLvol", "SP500_HLvol", "NewsTone", "Goldstein", "IAI", "DAI1", "DAI2", "DAI3", "BCI", "CCI", "CLI", "Flights", "Tempo", "Energy", "Danceability", "MoonPhase", "WkDay", "YrWeek", "Year")
  selectedVbles <- c("VIX", "VVIX", "VIX3M", "VIXNsdq", "GoldVlty", "DAI3", "CCI")
  calendarVbles <- c("MoonPhase", "WkDay", "YrWeek", "Year")
  df_planetMood_1 <- df_planetMood %>% 
    select(date, 
           selectedVbles, 
           calendarVbles)
  return(df_planetMood_1)  
}