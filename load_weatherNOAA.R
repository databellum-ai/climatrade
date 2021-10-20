# API de NOAA.gov
# https://www.rdocumentation.org/packages/rnoaa/versions/0.2.0
# Locations list: https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt
# https://docs.opendata.aws/noaa-ghcn-pds/readme.html

# 10 ciudades que dirigen la economía mundial: https://cincodias.elpais.com/cincodias/2007/06/13/sentidos/1181701636_850215.html
# https://www.math.u-bordeaux.fr/~arichou/site/tutorials/rnoaa_tutorial.html
# Email:	jes@databellum-ai.com
# Token:	fECFMxqLfSEuoVkfmRkUCGMALcIRvQey


if(!require(rnoaa)) install.packages("rnoaa", repos = "http://cran.us.r-project.org")

library('rnoaa')
options(noaakey = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
source("clavesAPI_noaa.R")

# Get table of all datasets
ncdc_datasets()
# Get data category data and metadata
ncdc_datacats(stationid='GHCND:SP000003195')
# Get data category data and metadata
ncdc_datacats(stationid='GHCND:SP000003195')
# Fetch list of city locations in descending order
ncdc_locs(locationcategoryid='CITY', sortfield='name', sortorder='asc')


# Get info on a station by specifcying a dataset, locationtype, location, and station
ncdc_stations(datasetid='GHCND', locationid='FIPS:12017', stationid='GHCND:USC00084289')
# Search for data
ncdc(datasetid='GHCND', stationid='GHCND:SP000003195', datatypeid='TMAX', startdate = '2010-01-01', enddate = '2010-01-31', limit=500)


# # Specify datasetid, locationidtype, locationid, and station
ncdc_stations(datasetid='GHCND', locationid='FIPS:12017', stationid='GHCND:USC00084289')
ncdc_stations(datasetid='GHCND', stationid='GHCND:SP000003195') # Madrid-Retiro
ncdc_stations(datasetid='GHCND', stationid='GHCND:SPE00119828') # Oviedo




# ==================================================


# https://www.ncei.noaa.gov/access/search/dataset-search?observationTypes=Land%20Surface&startDate=2010-01-01T00:00:00&endDate=2021-01-01T23:59:59