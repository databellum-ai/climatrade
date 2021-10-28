# ===========================================================
# ===========================================================
# ===========================================================
# PTE: Obtener estaciones a partir de coordenadas de ciudades: https://rdrr.io/cran/rnoaa/man/isd_stations_search.html | https://rdrr.io/cran/rnoaa/man/meteo_nearby_stations.html
# PTE: Revisar datos y mirar un chart
# PTE: Parámetro para cargar la historia (STEP 1 de los CSV.GZ) de .RDS o recalcularla


year_from_NOAA <- 1989
refreshAlsoHistory <- FALSE

# ---------------------------
# ---------------------------
# STEP 1: get locations near relevent cities

# Read available stations
station_data <- readRDS("./data/stations_NOAA.rds")
# station_data <- ghcnd_stations() # Takes a while to run
# saveRDS(station_data, "./data/stations_NOAA.rds") # Save to RDS




# Define our target cities (to look for near stations near them)
  # 10 ciudades que dirigen la economía mundial: https://cincodias.elpais.com/cincodias/2007/06/13/sentidos/1181701636_850215.html
relevantCities <- data.frame(
  id = c("NewYork", "Oviedo", "Amsterdam", "Paris", "HongKong", "Chicago", "London", "LosAngeles", "Beijng", "Madrid", "Albacete", "Frankfurt", "Tokyo", "Seoul", "Singapore", "Dubai"),
  latitude = c(40.70623940806975, 43.3620683921967, 52.37288021193839, 48.8613182352403, 22.32029644568666, 41.88632188372439, 51.50702741724013, 34.05084926622552, 39.905384001792335, 40.425619645599916, 39.267266932791685, 50.12095925753092, 35.687667406759765, 37.568428507484775, 1.2929342653888358, 25.214919588761404), 
  longitude = c(-74.00883633105707, -5.84817121485434, 4.896615844580131, 2.3003412809927495, 114.19091287611904, -87.67086062661967, -0.12701173276875632, -118.25389861960555, 116.37699181234836, -3.7025627487487984, -1.5500112927257998, 8.637929689001126, 139.76554769212072, 126.9780904050825, 103.84642630591777, 55.2762538818061))
relevantCities




# ~~~~~~~~~~~~~~~~~~~~~~~~
# https://rdrr.io/cran/rnoaa/man/
# tmp2 <- isd_stations(refresh = FALSE)
# tmp2


station_data
stationsPeriods <- station_data %>% 
  filter(element %in% c("TMAX", "TMIN", "PRCP")) %>% 
  group_by(id, first_year, last_year) %>% summarise()
stationsPeriods
from_y <- year_from_NOAA # initial year (parameter)
to_y <- as.character(year(Sys.Date())) # current year

# >>>>>>>>>>>>
# >>>>>>> LOOP
i <- 1
i_city <- relevantCities$id[i]
i_lat <- relevantCities$latitude[i]
i_lon <- relevantCities$longitude[i]

c_radius <- 50
c_limit <- 10
tmpAroundStations <- meteo_nearby_stations(lat_lon_df = relevantCities[i,], 
                                     station_data = station_data,
                                     radius = c_radius, 
                                     limit = c_limit, 
                                     var = c("PRCP", "TMIN", "TMAX"),
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
# make data binary
tmpYearsCoveredPerNearbyStation[yr_columns] <- ifelse(tmpYearsCoveredPerNearbyStation[yr_columns]>0, TRUE, FALSE)



tmpYearsCoveredPerNearbyStation[1:10,2:5]
tmpYearsCoveredPerNearbyStation[1,2:5] & tmpYearsCoveredPerNearbyStation[1,2:5]


nAvailStations <- nrow(tmpYearsCoveredPerNearbyStation)
nUsedStations <- 3
assessCombs_df <- combinations(n = nStations, r = nUsedStations, tmpYearsCoveredPerNearbyStation$id, repeats=FALSE)
assessCombs_df
dim(assessCombs_df)


j <- 1
tmpYearsCoveredPerNearbyStation %>% filter(id %in% assessCombs_df[j,]) %>% select(-id)









tmpAroundStations <- isd_stations_search(lat = i_lat, lon = i_lon, radius = c_radius)

beginDates <- ymd(as.character(tmpAroundStations$begin))
endDates <- ymd(as.character(tmpAroundStations$end))

monitors <- c("ASN00095063", "ASN00024025", "ASN00040112", "ASN00041023",
              "ASN00009998", "ASN00066078", "ASN00003069", "ASN00090162",
              "ASN00040126", "ASN00058161")
obs <- meteo_pull_monitors(monitors)
obs_covr <- meteo_coverage(obs)
obs_covr

meteo_distance(station_data, -33, 151, radius = 10, limit = 10)

# ~~~~~~~~~~~~~~~~~~~~~~~~






# Obtain stations near our cities (<50km and up to 5 per city)
tmpStations <- meteo_nearby_stations(lat_lon_df = relevantCities, 
                                     station_data = station_data,
                                     radius = 50, 
                                     limit = 5, 
                                     var = c("PRCP", "TMIN", "TMAX"),
                                     year_min = 1989, 
                                     year_max = 2021)
class(tmpStations)
names(tmpStations)

# Prepare lists of Cities and Stations to use later
stationsNames <- NULL
stationsIds <- NULL
for (tmpCity in relevantCities$id) {
  stationsNames <- c(stationsNames, rep(tmpCity, length(tmpStations[[tmpCity]]$id)))
  stationsIds <- c(stationsIds,tmpStations[[tmpCity]]$id)
}
stationsNames
stationsIds
# ---------------------------
# ---------------------------


# ---------------------------
# ---------------------------
# STEP 2 (historical): leyendo cada .csv histórico
baseURL <- 'https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/by_station/'
historicStationsData <- data.frame()
for (i_station in 1:length(stationsIds)) {
  currentURL <- paste(baseURL, stationsIds[i_station],".csv.gz", sep="")
  print(currentURL)
  print(paste(stationsIds[i_station],stationsNames[i_station], sep=" - "))
  tmpStationData <- read_csv(
    file = currentURL, 
    col_names=FALSE)
  tmpStationData <- tmpStationData %>% 
    mutate(date = ymd(X2), 
           station = paste('GHCND:',stationsIds[i_station],sep=""), 
           stationPlace = stationsNames[i_station]) %>% 
    filter(year(date) >= year_from_NOAA) %>% 
    filter(X3 %in% c("TMIN", "TMAX", "PRCP")) %>% 
    select(date, station, stationPlace, indicator = X3, value = X4)
  historicStationsData <- rbind(historicStationsData, tmpStationData)
}
head(historicStationsData)
# ---------------------------
# ---------------------------


# ---------------------------
# ---------------------------
# STEP 3 (incremental): mediante el paquete RNOAA
# API de NOAA.gov
# https://www.rdocumentation.org/packages/rnoaa/versions/0.2.0
# Locations list: https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt
# https://docs.opendata.aws/noaa-ghcn-pds/readme.html
# https://www.math.u-bordeaux.fr/~arichou/site/tutorials/rnoaa_tutorial.html


# Get data only since last available load
loadDate <- as.character(Sys.Date()) # today
firstDateUnavailable <- as.character(max(historicStationsData$date)+1) # use preloaded data to get incrementally
# Search for recent/new data
for (i_station in stationsIds) {
  print(paste(i_station,stationsNames[which(stationsIds == i_station)], sep=" - "))
  recentStationTMIN <- ncdc(datasetid='GHCND', 
                         stationid=paste('GHCND:',i_station,sep=""), 
                         datatypeid='TMIN', 
                         startdate = firstDateUnavailable, enddate = loadDate, 
                         sortfield = 'date', 
                         limit=366)
  recentStationTMAX <- ncdc(datasetid='GHCND', 
                         stationid=paste('GHCND:',i_station,sep=""), 
                         datatypeid='TMAX', 
                         startdate = firstDateUnavailable, enddate = loadDate, 
                         sortfield = 'date', 
                         limit=366)
  recentStationPRCP <- ncdc(datasetid='GHCND', 
                         stationid=paste('GHCND:',i_station,sep=""), 
                         datatypeid='PRCP', 
                         startdate = firstDateUnavailable, enddate = loadDate, 
                         sortfield = 'date', 
                         limit=366)
  recentStationData <- rbind(recentStationTMIN$data, recentStationTMAX$data, recentStationPRCP$data)
  if (nrow(recentStationData) == 0) {
    recentStationData <- historicStationsData[FALSE,]  # create an empty dataframe with same columns to append
  } else {
    recentStationData <- recentStationData %>% 
      mutate(date = date(date), 
             stationPlace = stationsNames[which(stationsIds == i_station)], 
             indicator=datatype) %>% 
      select(date, station, stationPlace, indicator, value)
  }
  allStationsData <- rbind(historicStationsData, recentStationData)
}
head(allStationsData)

# Average by date/city/indicator (in case multiple stations in a city), Spread columns per indicator, and transform temperature to integer celsius degrees
allStationsData <- allStationsData %>%
  group_by(date, stationPlace, indicator) %>% 
  summarize(value = mean(value)) %>% 
  spread(key = indicator, value = value) %>% 
  mutate(TMIN=round(TMIN/10,0), TMAX=round(TMAX/10,0), PRCP=round(PRCP,0))
head(allStationsData)

# Create columns combining indicator and place  
allStationsData <- allStationsData %>%
  pivot_wider(names_from = stationPlace, values_from = c("TMIN", "TMAX", "PRCP"), names_sep=".")
head(allStationsData)
names(allStationsData)

# Save to RDS
saveRDS(allStationsData, "./data/data_NOAA.rds")
write.csv(allStationsData, "./data/tmpNOAA.csv")


