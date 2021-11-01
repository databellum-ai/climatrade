# PTE: Rellenar valores TAVG que falten donde haya TMIN y TMAX
# PTE: Lectura directa de coordenadas (requiere API de Google)
  # if(!require(ggmap)) install.packages("ggmap", repos = "http://cran.us.r-project.org") # obtain city coordianates
  # library(ggmap)
  # geocode("New York City")


# ===========================================================
# EXTRACTION OF WEATHER DATA FROM NOAA BY CITY/PLACE
# We initially calculate stations nearby and optimize to reach maximum coverage years (Step 1). Then, we extract historical data from FTP files (Step 2). Finally, we add also most recent data read using NOAA API (Step 3)
# ===========================================================

# ---------------------------------------------
# STEP 1: get optimal stations near relevant cities
# ---------------------------------------------
# Extended list of relevant cities:
# relevantCities <- data.frame(
#   id = c("NewYork", "Oviedo", "Amsterdam", "Paris", "HongKong", "Chicago", "London", "LosAngeles", "Beijng", "Madrid", "Albacete", "Frankfurt", "Tokyo", "Seoul", "Singapore", "Dubai"),
#   latitude = c(40.70623940806975, 43.3620683921967, 52.37288021193839, 48.8613182352403, 22.32029644568666, 41.88632188372439, 51.50702741724013, 34.05084926622552, 39.905384001792335, 40.425619645599916, 39.267266932791685, 50.12095925753092, 35.687667406759765, 37.568428507484775, 1.2929342653888358, 25.214919588761404),
#   longitude = c(-74.00883633105707, -5.84817121485434, 4.896615844580131, 2.3003412809927495, 114.19091287611904, -87.67086062661967, -0.12701173276875632, -118.25389861960555, 116.37699181234836, -3.7025627487487984, -1.5500112927257998, 8.637929689001126, 139.76554769212072, 126.9780904050825, 103.84642630591777, 55.2762538818061))
relevantCities

measureTypes <- c("TMAX", "TMIN", "TAVG", "PRCP") # indicators we want to read

# Read available stations
station_data <- readRDS("data/stations_NOAA.rds")
# station_data <- ghcnd_stations() # Takes a while to run
# saveRDS(station_data, "data/stations_NOAA.rds") # Save to RDS

station_data
stationsPeriods <- station_data %>% 
  filter(element %in% measureTypes) %>% 
  group_by(id, first_year, last_year) %>% summarise()
stationsPeriods


# >>>>>>>>>>>>>>>>>
# >>>>>>> LOOP CITY
stationsNames <- NULL
stationsIds <- NULL
for (i in c(1:nrow(relevantCities))) {
i_city <- relevantCities$id[i]
tmpAroundStations <- meteo_nearby_stations(lat_lon_df = relevantCities[i,], 
                                     station_data = station_data,
                                     radius = c_radius, 
                                     limit = c_limit, 
                                     var = measureTypes,
                                     year_min = from_y, 
                                     year_max = to_y)
tmpAroundStations <- as.tibble(tmpAroundStations[[1]]$id) %>% rename(id = value)
tmpAroundStations
stationsPeriods
# add availability years for stations hereby
tmpYearsCoveredPerNearbyStation <- tmpAroundStations %>% 
  left_join(stationsPeriods)
# add columns to data frame to contain each year's data availability
yr_columns <- as.character(c(from_y:to_y))
tmpYearsCoveredPerNearbyStation <- tmpYearsCoveredPerNearbyStation %>% 
  cbind(setNames( lapply(yr_columns, function(x) x=FALSE), yr_columns) ) 
# fill TRUE/FALSE depending on stations's data availability each year
tmpYearsCoveredPerNearbyStation <- tmpYearsCoveredPerNearbyStation %>%
  mutate(across(yr_columns, ~ ifelse( (cur_column() >= first_year & cur_column() <= last_year), TRUE , FALSE)))
# keep only location and availabilities 
tmpYearsCoveredPerNearbyStation <- tmpYearsCoveredPerNearbyStation %>% 
  select(-first_year,-last_year)
# sum all available periods within each station
tmpYearsCoveredPerNearbyStation <- tmpYearsCoveredPerNearbyStation %>% 
  group_by(id) %>% 
  summarise_all(sum)
# calculate all combinations of locations we might use
nAvailStations <- nrow(tmpYearsCoveredPerNearbyStation)
if (nAvailStations < nUsedStations) {
  nUsedStations <- nAvailStations
}
assessCombs_df <- 
  combinations(n = nAvailStations, r = nUsedStations, tmpYearsCoveredPerNearbyStation$id, repeats=FALSE)
assessCombs_df
dim(assessCombs_df)
nCombinations <- nrow(assessCombs_df)
nCombinations

# for each combination we extract how many years are covered
o_allPeriod<- NULL
o_5years <- NULL
o_combination <- NULL
for (j in 1:nCombinations) {
  tmpGroup <- tmpYearsCoveredPerNearbyStation %>% filter(id %in% assessCombs_df[j,]) %>% select(-id)
  # Years covered
  tmpYearlyAvailability <- ifelse(colSums(tmpGroup)>0, TRUE, FALSE)
  tmpCoveredPercentageAllPeriod <- mean(tmpYearlyAvailability)
  tmpCoveredPercentageLast5Years <- mean(tail(tmpYearlyAvailability, n=5))
  o_allPeriod <- c(o_allPeriod, tmpCoveredPercentageAllPeriod)   
  o_5years <- c(o_5years, tmpCoveredPercentageLast5Years)
  o_combination <- c(o_combination, j)
}


cityResults <- data.frame(o_allPeriod, o_5years) %>% 
  mutate(ord = o_combination) %>% 
  arrange(desc(o_5years), desc(o_allPeriod)) 
cityResults

print("vvvvvvvvvvvvv")
print(i_city)
print(cityResults[1,])
print(nrow(cityResults[1,]))
print(nrow(assessCombs_df))

chosenId <- cityResults[1,]$ord
print(assessCombs_df[chosenId,])

stationsNames <- c(stationsNames, replicate(n = length(assessCombs_df[chosenId,]), i_city))
stationsIds <- c(stationsIds, assessCombs_df[chosenId,])
print("^^^^^^^^^^^^")

}
# <<<<< END LOOP CITY
# <<<<<<<<<<<<<<<<<<

print("Cities:")
print(stationsNames)
print("Selected statations (groups of stations with highest aggregated coverage per city):")
print(stationsIds)



# ---------------------------------------------
# STEP 2: get historical data from NOAA FTP files
# ---------------------------------------------
baseURL <- 'https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/by_station/'
historicStationsData <- data.frame()
print("Reading historical (.csv.gz) values")
for (i_station in 1:length(stationsIds)) {
  currentURL <- paste(baseURL, stationsIds[i_station],".csv.gz", sep="")
  print(paste("Station: ",stationsIds[i_station]," (",stationsNames[i_station],")",sep=""))
  tmpStationData <- read_csv(
    file = currentURL, 
    col_names=FALSE)
  tmpStationData <- tmpStationData %>% 
    mutate(date = ymd(X2), 
           station = paste('GHCND:',stationsIds[i_station],sep=""), 
           stationPlace = stationsNames[i_station]) %>% 
    filter(year(date) >= year_from_NOAA) %>% 
    filter(X3 %in% measureTypes) %>% 
    select(date, station, stationPlace, indicator = X3, value = X4)
  historicStationsData <- rbind(historicStationsData, tmpStationData)
}
head(historicStationsData)



# ---------------------------------------------
# STEP 3 (incremental): get recent data using NOAA API
# ---------------------------------------------
# API de NOAA.gov API: https://www.rdocumentation.org/packages/rnoaa/versions/0.2.0
# Get data only since last available load
loadDate <- as.character(Sys.Date()) # today
overlapRecent_Historical <- 15  # number of days we read recent data (API) despite they those dates were contained in historical (.csv.gz), i.e. just in case historical recent values are still volatile
firstDateUnavailable <- as.character(max(historicStationsData$date)-overlapRecent_Historical) # use preloaded data to get incrementally, though allowing some overlap

allStationsData <- historicStationsData
recentStationData <- data.frame()
print("Reading recent (API) values")
# Search for recent/new data
for (i_station in stationsIds) {
  recentStationData <- ncdc(datasetid='GHCND', 
                            stationid=paste('GHCND:',i_station,sep=""), 
                            datatypeid=c("TMIN","TMAX","TAVG","PRCP"), 
                            startdate = firstDateUnavailable, enddate = loadDate, 
                            sortfield = 'date', 
                            limit=366)
  recentStationData <- recentStationData$data
  print(paste("Station: ", i_station, " (", stationsNames[which(stationsIds == i_station)], ") - ", nrow(recentStationData)," values found",sep=""))  
  if (nrow(recentStationData) == 0) {
    recentStationData <- historicStationsData[FALSE,]  # create an empty dataframe with same columns to append
  } else {
    recentStationData <- recentStationData %>% 
      mutate(date = date(date), 
             stationPlace = stationsNames[which(stationsIds == i_station)], 
             indicator=datatype) %>% 
      select(date, station, stationPlace, indicator, value)
  }
  allStationsData <- rbind(allStationsData, recentStationData)
}
print("Merging historical (.csv.gz) and recent (API) values")

# Average by date/city/indicator (in case multiple stations in a city), , and 
allStationsData <- allStationsData %>%
  group_by(date, stationPlace, indicator) %>% 
  summarize(value = mean(value, na.rm=TRUE))
# Transform temperature to integer celsius degrees
allStationsData <- allStationsData %>% 
  mutate(value = ifelse(indicator %in% c("TMIN", "TMAX", "TAVG"), round(value/10,0), value))
head(allStationsData)



# ---------------------------------------------
# STEP: Save
# ---------------------------------------------

# Save to RDS (times-series format)
saveRDS(allStationsData, "data/data_weather_ts.rds")

# Spread columns per indicator
allStationsData <- allStationsData %>% spread(key = indicator, value = value)
head(allStationsData)
# Create columns combining indicator and place as suffix  
allStationsData <- allStationsData %>% pivot_wider(names_from = stationPlace, values_from = c("TMIN", "TMAX", "TAVG", "PRCP"), names_sep=".",names_sort=TRUE)
head(allStationsData)
# Save to RDS (spread format)
saveRDS(allStationsData, "data/data_weather_sp.rds")


