# API de NOAA.gov
# https://www.rdocumentation.org/packages/rnoaa/versions/0.2.0
# Locations list: https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt
# https://docs.opendata.aws/noaa-ghcn-pds/readme.html

# 10 ciudades que dirigen la economía mundial: https://cincodias.elpais.com/cincodias/2007/06/13/sentidos/1181701636_850215.html
# https://www.math.u-bordeaux.fr/~arichou/site/tutorials/rnoaa_tutorial.html


# ---------------------------
# ---------------------------
# LEYENDO DE LAS ESTACIONES DIRECTAMENTE
# https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/
# https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/by_station/

# AEM00041194	DUBAI INTL
# CHM00054511	BEIJING
# FRM00007156	PARIS-MONTSOURIS
# GME00122362	FRANKFURT/MAIN-WESTEND
# JA000047662	TOKYO
# KSM00047108	SEOUL CITY
# MCM00045011	MACAU INTL
# NLE00152485	SCHIPHOL
# SNM00048698	SINGAPORE CHANGI INTL
# SP000008280	ALBACETE LOS LLANOS
# SPE00119828	OVIEDO
# SPE00120278	MADRID/BARAJAS
# UKM00003772	HEATHROW
# USC00111577	CHICAGO MIDWAY AP 3SW
# USW00014732	NEW YORK LAGUARDIA AP
# USW00093134	LOS ANGELES DWTN USC CAMPUS
# USW00094728	NEW YORK CNTRL PK TWR
# USW00094846	CHICAGO OHARE INTL AP



stationData <- read_csv(file = "https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/by_station/AGM00060387.csv.gz")


# ---------------------------
# ---------------------------

if(!require(rnoaa)) install.packages("rnoaa", repos = "http://cran.us.r-project.org")

library('rnoaa')
options(noaakey = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
source("clavesAPI_noaa.R")

# Get table of all datasets
ncdc_datasets()
# Get data category data and metadata
ncdc_datacats(stationid='GHCND:SP000003195')


# Search for data in Station in Year
stationsFinanceCapitals <- c("SP000003195", "SPE00119828")
stationsData <- data.frame()
for (i_station in stationsFinanceCapitals) {
  print(i_station)
  tmpStationTMIN <- ncdc(datasetid='GHCND', stationid=paste('GHCND:',i_station,sep=""), datatypeid='TMIN', startdate = '2010-01-01', enddate = '2010-12-31', sortfield = 'date', limit=366)
  tmpStationTMAX <- ncdc(datasetid='GHCND', stationid=paste('GHCND:',i_station,sep=""), datatypeid='TMAX', startdate = '2010-01-01', enddate = '2010-12-31', sortfield = 'date', limit=366)
  tmpStationPRCP <- ncdc(datasetid='GHCND', stationid=paste('GHCND:',i_station,sep=""), datatypeid='PRCP', startdate = '2010-01-01', enddate = '2010-12-31', sortfield = 'date', limit=366)
  tmpStationData <- rbind(tmpStationTMIN$data, tmpStationTMAX$data, tmpStationPRCP$data)
  tmpStationData <- tmpStationData %>% 
    group_by(date, datatype, station) %>% summarize(value) %>% 
    spread(key = datatype, value = value) %>% select(date, station, TMIN, TMAX, PRCP)
  stationsData <- rbind(stationsData, tmpStationData)
}
#FALTA: meter todas la Stations + Loop por año + Convertir a Fahrenheit + Unidades PRCP? + salvar&carga incremental

head(stationsData)
count(stationsData)
unique(stationsData$date)
unique(stationsData$station)





tmpStationTMAX <- tmpStationTMAX$data %>% mutate(TMAX = value) %>% select(station, date, TMAX)


head(tmplocationData)


# ==================================================


# https://www.ncei.noaa.gov/access/search/dataset-search?observationTypes=Land%20Surface&startDate=2010-01-01T00:00:00&endDate=2021-01-01T23:59:59