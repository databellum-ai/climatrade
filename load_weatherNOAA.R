# PTE:
# -Repasar fiabilidad/regularidad de datos de cada estación y reeemplazar si es necesario
# -Probar RNOAA para extracción incremental de fechas muy recientes (chequear el delay de los .CSV)



# 10 ciudades que dirigen la economía mundial: https://cincodias.elpais.com/cincodias/2007/06/13/sentidos/1181701636_850215.html
stationsIds <- c("USW00094728", "FRM00007156", "MCM00045011", "USW00094846", "UKM00003772", "USW00093134", "CHM00054511", "SPE00120278", "SP000008280", "SPE00119828")
stationsNames <- c("NewYork", "Paris", "HongKong", "Chicago", "London", "LosAngeles", "Beijng", "Madrid", "Albacete", "Oviedo")

# FALLIDA:
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
           stationPlace = stationsNames[i_station]) %>% 
    filter(year(date) >= year_from) %>% 
    filter(X3 %in% c("TMIN", "TMAX", "PRCP")) %>% 
    select(date, stationPlace, indicator = X3, value = X4)
  allStationsData <- rbind(allStationsData, tmpStationData)
}
head(allStationsData)
# Separate columns per indicator and transform temperature de celsius
allStationsData <- allStationsData %>% 
  spread(key = indicator, value = value) %>% 
  mutate(TMIN=round(TMIN/10,0), TMAX=round(TMAX/10,0) )


# Create columns combining indicator and place  
# https://stackoverflow.com/questions/53849240/tidyrspread-with-multiple-keys-and-values
allStationsData <- allStationsData %>%
  pivot_wider(names_from = stationPlace, values_from = c("TMIN", "TMAX", "PRCP"), names_sep="_")
head(allStationsData)

# Save to RDS
saveRDS(allStationsData, "./data/weatherNOAA.rds")
# write.csv(allStationsData, "./data/weatherNOAA.csv", row.names=TRUE)


summary(allStationsData$date)


# ---------------------------
# ---------------------------
# STEP 2 (incremental): mediante el paquete RNOAA
# API de NOAA.gov
# https://www.rdocumentation.org/packages/rnoaa/versions/0.2.0
# Locations list: https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt
# https://docs.opendata.aws/noaa-ghcn-pds/readme.html
# https://www.math.u-bordeaux.fr/~arichou/site/tutorials/rnoaa_tutorial.html

options(noaakey = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
source("clavesAPI_noaa.R")

historicalData <- readRDS("./data/weatherNOAA.rds") # previously loaded data (STEP 1)

# Get data only since last historic load
loadDate <- as.character(Sys.Date()) # today
firstDateUnavailable <- as.character(max(historicalData$date)+1) # use preloaded data to get incrementally

#!!!!! OJO: ESTOY DUPLICANDO POR LEER DESDE TAN ATRÁS PARA QUE NO FALLE
firstDateUnavailable <- as.character(Sys.Date()-180)

# Search for data in Station in Year
stationsData <- data.frame()
for (i_station in stationsIds) {
  print(i_station)
  tmpStationTMIN <- ncdc(datasetid='GHCND', stationid=paste('GHCND:',i_station,sep=""), datatypeid='TMIN', startdate = firstDateUnavailable, enddate = loadDate, sortfield = 'date', limit=366)
  tmpStationTMAX <- ncdc(datasetid='GHCND', stationid=paste('GHCND:',i_station,sep=""), datatypeid='TMAX', startdate = firstDateUnavailable, enddate = loadDate, sortfield = 'date', limit=366)
  tmpStationPRCP <- ncdc(datasetid='GHCND', stationid=paste('GHCND:',i_station,sep=""), datatypeid='PRCP', startdate = firstDateUnavailable, enddate = loadDate, sortfield = 'date', limit=366)
  tmpStationData <- rbind(tmpStationTMIN$data, tmpStationTMAX$data, tmpStationPRCP$data)
  tmpStationData <- tmpStationData %>% 
    group_by(date, datatype, station) %>% summarize(value) %>% 
    spread(key = datatype, value = value) %>% select(date, station, TMIN, TMAX, PRCP)
  stationsData <- rbind(stationsData, tmpStationData)
}
#FALTA: arreglar fallo por no datos + sustituir estaciones fallidas + incremental fusionar con carga histórica

head(stationsData)
unique(stationsData$date)
unique(stationsData$station)

tmpStationTMAX <- tmpStationTMAX$data %>% mutate(TMAX = value) %>% select(station, date, TMAX)

head(tmplocationData)
