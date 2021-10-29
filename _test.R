# Average by date/city/indicator (in case multiple stations in a city), Spread columns per indicator, and transform temperature to integer celsius degrees
# allStationsData2 <- 


allStationsData2 %>% filter(date=="1989-01-01" & stationPlace %in% c("Amsterdam", "Beijing", "Frankfurt")) %>% 
  head()

ncdc(datasetid='GHCND', 
     stationid=paste('GHCND:','NLE00100503',sep=""), 
     startdate = "1989-01-01", enddate = "1989-01-01", 
     sortfield = 'date', 
     limit=366)
ncdc(datasetid='GHCND', 
     stationid=paste('GHCND:','NLE00101920',sep=""), 
     startdate = "1989-01-01", enddate = "1989-01-01", 
     sortfield = 'date', 
     limit=366)
ncdc(datasetid='GHCND', 
     stationid=paste('GHCND:','NLE00101926',sep=""), 
     startdate = "1989-01-01", enddate = "1989-01-01", 
     sortfield = 'date', 
     limit=366)

allStationsData2 %>% filter(date=="1989-01-01" & stationPlace %in% c("Amsterdam", "Beijing", "Frankfurt")) %>% 
  group_by(date, stationPlace, indicator) %>% 
  summarize(value = mean(value)) %>% 
  spread(key = indicator, value = value) %>% 
  mutate(TMIN=round(TMIN/10,0), TMAX=round(TMAX/10,0), PRCP=round(PRCP,0)) %>% 
  head()



