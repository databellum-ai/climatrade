# =====================================================
# EXTRACTION OF MOON PHASE, SUNRISE/SUNSET/NIGHT_HOURS AND TIDES
# =====================================================

library(tidyverse)
library(lubridate)


# =====================================================
# MOON PHASE, SUNRISE/SUNSET/NIGHT_HOURS DAILY
# =====================================================
# https://cran.r-project.org/web/packages/suncalc/suncalc.pdf

initialDate <- "1979-01-01"
allPossibleDates <- seq(ymd(initialDate, tz = "UTC"), as.POSIXct(Sys.Date()), by="days")
allPossibleDates

moonData <- getMoonIllumination(date = allPossibleDates, keep = c("phase"))
moonData <- moonData %>% mutate(date = as.Date(moonData$date)) %>% rename(moonPhase = phase)
moonData
# Moon phase value varies from 0.0 to 1.0 and should be interpreted like this: 
#   0 New Moon
#    Waxing Crescent
#   0.25 First Quarter
#    Waxing Gibbous
#   0.5 Full Moon
#    Waning Gibbous
#   0.75 Last Quarter
#    Waning Crescent


# =====================================================
# ADDING DAY-OF-WEEK AND WEEK-OF-YEAR
# =====================================================
# Although sun data is per city and moon phase is at world level, we join it all in a single "denormalized" list for simplification

# We add Weekday and reorder columns
moonData <- moonData %>% 
  mutate(weekday = wday(date), 
         yearWeek = week(ymd(date)), 
         countryCode = NA) %>% 
  select(date, moonPhase, weekday, yearWeek, countryCode)
head(moonData)

# ================================
# SAVE
# ================================
# Save dataset
saveRDS(moonData,"data/data_calendarMoon_ts.rds")

print("End of calendar and Moon phase extraction process")

