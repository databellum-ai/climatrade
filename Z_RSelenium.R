# PTE: corregir grabación del df: tracksJustFound
# PTE: ejecución estable bucles: regular el delay + resiliente a fallos de carga
# PTE: pasar login
# PTE: integrar con original

library(tidyverse)
library(rvest)
library(RSelenium)
library(binman)

# Determine what dates we'll process:
unProcessedDates <- NULL
allPossibleDates <- seq(as.Date(musicInitialDate), Sys.Date()-1, by="days")
allPossibleDates
# read already available data to ensure claculation only of delta. At the end we'll consolidate
if (file.exists("data/data_music_ts.rds")) {
  historicTracksFeatures <- readRDS("data/data_music_ts.rds")
  # get a list of dates appearing in the website but not in our historic record and use it for further process
  existingDates <- historicTracksFeatures %>% group_by(date) %>% summarize()
  existingDates$date <- as.Date(existingDates$date)
  unProcessedDates <- allPossibleDates[!(allPossibleDates %in% existingDates$date)]
  unProcessedDates
} else {
  # in case there is no history of data, we must process all available (on the web) dates
  historicTracksFeatures <- data.frame()
  unProcessedDates <- allPossibleDates
}
# to process only a limited number of recent dates:
if (length(unProcessedDates) <= lotSize) {
  unProcessedDates <- unProcessedDates[1:length(unProcessedDates)] 
} else { 
  unProcessedDates <- unProcessedDates[1:lotSize]
}
unProcessedDates

# Determine what countries we'll process:
allPossibleCountries <- readRDS("data/geo_music.rds")
allPossibleCountries

# https://thatdatatho.com/tutorial-web-scraping-rselenium/


#Shut Down Client and Server
# remote_driver$close()
# driver$server$stop()
# system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
list_versions("chromedriver")
driver <- rsDriver(version = "latest", browser=c("chrome"), chromever = "96.0.4664.45")
remote_driver <- driver[["client"]]


numTopTracks <- 3
listenedTracks <- data.frame()

# Ensure login
url <- paste("https://charts.spotify.com/charts/overview/global")
remote_driver$navigate(url)

# LOOPING dates
for (i in c(19:19)) {
  # for (i in c(1:length(unProcessedDates))) {
  print(unProcessedDates[i])
  print("----------")
  # LOOPING all countries
  for (j in c(1:nrow(allPossibleCountries))) {
    print(allPossibleCountries$countryCode[j])
    url <- paste("https://charts.spotify.com/charts/view/regional-",allPossibleCountries$countryCode[j],"-daily/",unProcessedDates[i],sep="")
    remote_driver$navigate(url)
    # Delay to wait until page loads:
    #Sys.sleep(0.1)  
    print(paste("URL:", url))
    pageNotLoaded <- length(remote_driver$findElements(using='xpath', '//*[@id="__next"]/div/div/main/div[2]/div[2]/div/h3')) != 0
    print(paste("Not loaded?:", pageNotLoaded))
    if (!pageNotLoaded) { # Check no load error
      staticVersion <- remote_driver$getPageSource()[[1]]
      spotify_tracks <- read_html(staticVersion) # rvest
      track <- NULL
      numStreams <- NULL
      # LOOPING top tracks
      for (k in c(1:numTopTracks)) {
        track[k] <- spotify_tracks %>% 
          html_nodes(xpath = 
                       paste('//*[@id="__next"]/div/div/main/div[2]/div[3]/div/table/tbody/tr[',k,']/td[3]/div/div[1]/a',sep="")) %>% 
          html_attr("href")
        track[k] <- str_remove(track[k],"https://open.spotify.com/track/")
        numStreams[k] <- spotify_tracks %>% 
          html_nodes(xpath = 
                       paste('//*[@id="__next"]/div/div/main/div[2]/div[3]/div/table/tbody/tr[',k,']/td[7]',sep="")) %>% html_text() 
        numStreams[k] <- as.numeric(gsub(",", "", numStreams[k]))
        print(paste("k =",k,"| track ->",track[k]))
        print(paste("k =",k,"| numStreams ->",numStreams[k]))  
      }  # ^^ TRACKS
      tracksJustFound <-
        data.frame(trackId = track[1:numTopTracks],
                   numStreams = numStreams[1:numTopTracks],
                   date = unProcessedDates[i],
                   country = allPossibleCountries$country,
                   countryCode = allPossibleCountries$countryCode)
      listenedTracks <- rbind(listenedTracks, tracksJustFound)
    }
  }
}

head(listenedTracks)







k <- 3
spotify_tracks %>% html_nodes(xpath = paste('//*[@id="__next"]/div/div/main/div[2]/div[3]/div/table/tbody/tr[',k,']/td[3]/div/div[1]/a',sep="")) %>% html_attr("href")


staticVersion <- remote_driver$getPageSource()[[1]]
staticVersion
spotify_tracks <- read_html(staticVersion) # rvest
spotify_tracks
staticVersion %>% html_nodes(xpath = paste('//*[@id="__next"]/div/div/main/div[2]/div[3]/div/table/tbody/tr[1]/td[3]/div/div[1]/a',sep="")) %>% html_attr("href")




//*[@id="__next"]/div/div/main/div[2]/div[3]/div/table/tbody/tr[1]/td[3]/div/div[1]/a
//*[@id="__next"]/div/div/main/div[2]/div[3]/div/table/tbody/tr[1]/td[3]/div/div[2]/a

# ----------------------------------

# <div data-testid="region-filter" class="RegionSelect__OverviewFilter-ibo3np-0 ilDAmP"><span class="Trigger-sc-18ecbf7-0 cGQwPM Trigger-uvfeg5-0 jQtrTN RegionSelect__StyledDropdownTrigger-ibo3np-1 iBVLW"><div class="Container-s1cmq-0 fQlZAH"><button aria-haspopup="listbox" id="dropdown-toggle" aria-labelledby="dropdown-label dropdown-toggle" aria-expanded="false" class="Button-sc-18tio8-0 jPCnoE">Global</button><svg role="img" focusable="false" height="16" width="16" viewBox="0 0 24 24" aria-hidden="true" class="Svg-sc-1usfroi-0 jhoCoB Arrow-sc-62daq7-0 dKKkZe"><polyline points="20 8 12 17 4 8" fill="none" stroke="#181818"></polyline></svg></div></span></div>

# <span class="Trigger-sc-18ecbf7-0 cGQwPM Trigger-uvfeg5-0 jQtrTN RegionSelect__StyledDropdownTrigger-ibo3np-1 iBVLW"><div class="Container-s1cmq-0 fQlZAH"><button aria-haspopup="listbox" id="dropdown-toggle" aria-labelledby="dropdown-label dropdown-toggle" aria-expanded="false" class="Button-sc-18tio8-0 jPCnoE">Australia</button><svg role="img" focusable="false" height="16" width="16" viewBox="0 0 24 24" aria-hidden="true" class="Svg-sc-1usfroi-0 jhoCoB Arrow-sc-62daq7-0 dKKkZe"><polyline points="20 8 12 17 4 8" fill="none" stroke="#181818"></polyline></svg></div></span>


test <- remote_driver$findElement(using = 'xpath', value = '//*[@id="dropdown-toggle"]')
test <- remote_driver$findElement(using = 'class', value = 'Container-s1cmq-0')

test <- remote_driver$findElement(using = 'xpath', value = '//*[@id="__next"]/div/div/main/div[2]/div[1]/header/div/div[2]/div/span/div')
view(test$getElementText())


test <- remote_driver$findElement(using = 'xpath', value = '//*[@id="__next"]/div/div/main/div[2]/div[1]/header/div/div[2]/div/span')
test$clickElement()
test$getActiveElement()

element <- unlist(remote_driver$findElement("class name", "Button-sc-18tio8-0")$getElementAttribute('class'))
length(element)


test <- remote_driver$findElement("data-testid", "region-filter")
test

buttonElem <- remote_driver$findElement("class name", "Button-sc-18tio8-0")
view(buttonElem$getElementText())
buttonElem$clickElement()

# -----------------------------------------


address_element <- remote_driver$findElement(using = 'class', value = 'width70')
address_element$sendKeysToElement(list("Lombard Street, San Francisco"))
button_element <- remote_driver$findElement(using = 'class', value = "button")
button_element$clickElement()
out <- remote_driver$findElement(using = "class", value="coordinatetxt")
lat_long <- out$getElementText()

# url <- "https://charts.spotify.com/charts/view/regional-global-daily/latest"