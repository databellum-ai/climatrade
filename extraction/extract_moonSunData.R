# =====================================================
# EXTRACTION OF MOON PHASE, SUNRISE/SUNSET/NIGHT_HOURS AND TIDES
# =====================================================


library(tidyverse)
library(lubridate)
library(tidygeocoder) # Resolve coordinates of cities/places
library(suncalc)

# Obtain coordinates of relevant cities
relevantCitiesCoordinates <- data.frame(name = cities, addr = addresses) %>% 
  geocode(addr) %>% 
  select(id = name, latitude = lat, longitude = long)
relevantCitiesCoordinates


# =====================================================
# MOON PHASE, SUNRISE/SUNSET/NIGHT_HOURS DAILY
# =====================================================
# https://cran.r-project.org/web/packages/suncalc/suncalc.pdf

initialDate <- "1960-01-01"
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
# SUNRISE/SUNSET/NIGHT_HOURS DAILY
# =====================================================
citySunData <- data.frame()
for (i in (1:nrow(relevantCitiesCoordinates))) {
  tmpCitySunData <- getSunlightTimes(date = moonData$date,
                                  keep = c("sunrise", "sunset"),
                                  lat = relevantCitiesCoordinates$latitude[i], 
                                  lon = relevantCitiesCoordinates$longitude[i], 
                                  tz = "UTC")
  print(countries[i])
  tmpCitySunData <- tmpCitySunData %>% 
    mutate(nightHours = (sunset-sunrise), countryCode = countries[i]) %>% select(-lat, -lon)
  citySunData <- rbind(citySunData, tmpCitySunData)
}
citySunData



# =====================================================
# JOIN MOON AND SUN DATA INTO A SINGLE DATASET
# =====================================================
# Although sun data is per city and moon phase is at world level, we join it all in a single "denormalized" list for simplification
head(moonData)
nrow(moonData)
head(citySunData)
nrow(citySunData)

moon_sunData <- left_join(citySunData, moonData)

# We add Weekday and reorder columns
moon_sunData <- moon_sunData %>% 
  mutate(weekday = weekdays(date, abbreviate = FALSE)) %>% 
  select("date", "countryCode", "weekday", "moonPhase", "sunrise", "sunset", "nightHours")

head(moon_sunData)
nrow(moon_sunData)

# ================================
# SAVE
# ================================
# Save dataset
saveRDS(moon_sunData,"data/data_moonSun_ts.rds")
# Save also countries and codes for further consolidation with other data
geo_moon_sun <- moon_sunData %>% group_by(countryCode) %>% summarise() %>% arrange(countryCode)
geo_moon_sun
saveRDS(geo_moon_sun,"data/geo_moonSun.rds")

