
# https://rdrr.io/cran/rnoaa/man/isd_stations_search.html
# https://rdrr.io/cran/rnoaa/man/meteo_nearby_stations.html

# ////////////////////////////////////////
# ////////////////////////////////////////

  # PTE:
# -Repasar fiabilidad/regularidad de datos de cada estación y reeemplazar si es necesario
# -Probar RNOAA para extracción incremental de fechas muy recientes (chequear el delay de los .CSV)
#-FALTA: arreglar fallo por no datos + sustituir estaciones fallidas + incremental fusionar con carga histórica

# 10 ciudades que dirigen la economía mundial: https://cincodias.elpais.com/cincodias/2007/06/13/sentidos/1181701636_850215.html
stationsIds <- c("USW00094728", "FRM00007156", "MCM00045011", "USW00094846", "UKM00003772", "USW00093134", "CHM00054511", "SPE00120278", "SP000008280", "SPE00119828")
stationsNames <- c("NewYork", "Paris", "HongKong", "Chicago", "London", "LosAngeles", "Beijng", "Madrid", "Albacete", "Oviedo")

# FALLIDAS:
# stationsIds <- c("GME00122362", "JA000047662", "KSM00047108", "SNM00048698", "AEM00041194", "SPE00120278", "SP000008280", "SPE00119828")
# stationsNames <- c("Frankfurt", "Tokyo", "Seoul", "Singapore", "Dubai", "Madrid", "Albacete", "Oviedo")

year_from <- 1989

# ---------------------------
# ---------------------------
# STEP 1 (historical): leyendo cada .csv histórico
baseURL <- 'https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/by_station/'
allStationsData <- data.frame()
for (i_station in 1:length(stationsIds)) {
  currentURL <- paste(baseURL, stationsIds[i_station],".csv.gz", sep="")
  print(currentURL)
  tmpStationData <- read_csv(
    file = currentURL, 
    col_names=FALSE)
  tmpStationData <- tmpStationData %>% 
    mutate(date = ymd(X2), 
           stationId = paste('GHCND:',stationsIds[i_station],sep=""), 
           stationPlace = stationsNames[i_station]) %>% 
    filter(year(date) >= year_from) %>% 
    filter(X3 %in% c("TMIN", "TMAX", "PRCP")) %>% 
    select(date, stationId, stationPlace, indicator = X3, value = X4)
  allStationsData <- rbind(allStationsData, tmpStationData)
}
head(allStationsData)

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
firstDateUnavailable <- as.character(max(allStationsData$date)+1) # use preloaded data to get incrementally
# In case we prefer to read from RDS:
  # historicalData <- readRDS("./data/weatherNOAA.rds") # previously loaded data (STEP 1)
  # firstDateUnavailable <- as.character(max(historicalData$date)+1) # use preloaded data to get incrementally

# Search for data in Station in Year
deltaStationsData <- data.frame()
for (i_station in stationsIds) {
  print(i_station)
  tmpStationTMIN <- ncdc(datasetid='GHCND', stationid=paste('GHCND:',i_station,sep=""), datatypeid='TMIN', startdate = firstDateUnavailable, enddate = loadDate, sortfield = 'date', limit=366)
  print(tmpStationTMIN)
  tmpStationTMAX <- ncdc(datasetid='GHCND', stationid=paste('GHCND:',i_station,sep=""), datatypeid='TMAX', startdate = firstDateUnavailable, enddate = loadDate, sortfield = 'date', limit=366)
  tmpStationPRCP <- ncdc(datasetid='GHCND', stationid=paste('GHCND:',i_station,sep=""), datatypeid='PRCP', startdate = firstDateUnavailable, enddate = loadDate, sortfield = 'date', limit=366)
  tmpStationData <- rbind(tmpStationTMIN$data, tmpStationTMAX$data, tmpStationPRCP$data)
  tmpStationData <- tmpStationData %>% 
  mutate(date = date(date), 
         stationId=station, 
         stationPlace = stationsNames[which(stationsIds == i_station)], indicator=datatype) %>% 
  select(date, stationId, stationPlace, indicator, value)
  deltaStationsData <- rbind(deltaStationsData, tmpStationData)
}


head(allStationsData)
head(deltaStationsData)



# ----------------------------------------------
# ----------------------------------------------
# ----------------------------------------------
# Spread TMIN, TMAX, PRCP 
tmpStationData <- tmpStationData %>%
  group_by(date, datatype, station) %>% summarize(value) %>%
  spread(key = datatype, value = value) %>% select(date, station, TMIN, TMAX, PRCP)

# Separate columns per indicator and transform temperature de integer celsius degrees
allStationsData <- allStationsData %>% 
  spread(key = indicator, value = value) %>% 
  mutate(TMIN=round(TMIN/10,0), TMAX=round(TMAX/10,0))
# Create columns combining indicator and place  
# https://stackoverflow.com/questions/53849240/tidyrspread-with-multiple-keys-and-values
allStationsData <- allStationsData %>%
  pivot_wider(names_from = stationPlace, values_from = c("TMIN", "TMAX", "PRCP"), names_sep="_")
head(allStationsData)
summary(allStationsData$date)

# Save to RDS
saveRDS(allStationsData, "./data/weatherNOAA.rds")
# write.csv(allStationsData, "./data/weatherNOAA.csv", row.names=TRUE)
