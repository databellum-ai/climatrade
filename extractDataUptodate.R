#========================================================
# Function to extract daily data since beginning until last close
extractDataUptodate <- function() {
  df_planetMood <- readRDS("data/df_planetMood.rds") %>% arrange(date) %>% 
    mutate(VIX = VIX+30, Year = year(date)) # Dataset ready for analysis 
  selectedVbles <- c("VIX", "VVIX", "VIX3M", "VIXNsdq", "GoldVlty", "DAI3", "CCI")
  calendarVbles <- c("MoonPhase", "WkDay", "YrWeek", "Year")
  extractDataUptodate <- df_planetMood %>% 
    select(date, 
           c("VIX", "VVIX", "VIX3M", "VIXNsdq", "GoldVlty", "DAI3", "CCI"), 
           c("MoonPhase", "WkDay", "YrWeek", "Year"))
  return(extractDataUptodate)  
}