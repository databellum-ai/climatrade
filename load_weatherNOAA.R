# PTE: Revisar datos y mirar un chart
# PTE: Parámetro para cargar la historia (STEP 1 de los CSV.GZ) de .RDS o recalcularla
# PTE: Obtener estaciones a partir de coordenadas de ciudades: https://rdrr.io/cran/rnoaa/man/isd_stations_search.html | https://rdrr.io/cran/rnoaa/man/meteo_nearby_stations.html


year_from_NOAA <- 1989


# 10 ciudades que dirigen la economía mundial: https://cincodias.elpais.com/cincodias/2007/06/13/sentidos/1181701636_850215.html
stationsIds <- c("USW00094728", "FRM00007156", "MCM00045011", "USW00094846", "UKM00003772", "USW00093134", "CHM00054511", "SPE00120278", "SP000008280", "SPE00119828", "GME00122362", "JA000047662", "KSM00047108", "SNM00048698", "AEM00041194")
stationsNames <- c("NewYork", "Paris", "HongKong", "Chicago", "London", "LosAngeles", "Beijng", "Madrid", "Albacete", "Oviedo", "Frankfurt", "Tokyo", "Seoul", "Singapore", "Dubai")

# ---------------------------
# ---------------------------
# STEP 1 (historical): leyendo cada .csv histórico
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
# STEP 2 (incremental): mediante el paquete RNOAA
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
  mutate(TMIN=round(TMIN/10,0), TMAX=round(TMAX/10,0))
head(allStationsData)

# Create columns combining indicator and place  
# https://stackoverflow.com/questions/53849240/tidyrspread-with-multiple-keys-and-values
allStationsData <- allStationsData %>%
  pivot_wider(names_from = stationPlace, values_from = c("TMIN", "TMAX", "PRCP"), names_sep=".")
head(allStationsData)
names(allStationsData)

# Save to RDS
saveRDS(allStationsData, "./data/data_NOAA.rds")
