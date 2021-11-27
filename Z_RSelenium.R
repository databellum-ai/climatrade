# PTE: pasar cookies
# PTE: integrar con original
# PTE: separar inicialización
# PTE: cambiar clave Spotify


numTopTracks <- 3
accountSpotify <- "jes@databellum-ai.com"
passwordSpotify <- "xxxxxx"

library(tidyverse)
library(rvest)
library(RSelenium)
library(binman)

# ---------------------------

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

# Ensure login
url <- paste("https://accounts.spotify.com/en/login?continue=https:%2F%2Fcharts.spotify.com%2Flogin")
remote_driver$navigate(url)
address_element <- remote_driver$findElement(using = 'xpath', value = '//*[@id="login-username"]')
password_element <- remote_driver$findElement(using = 'xpath', value = '//*[@id="login-password"]')
button_element <- remote_driver$findElement(using = 'xpath', value = '//*[@id="login-button"]')
address_element$clearElement()
password_element$clearElement()
address_element$sendKeysToElement(list(accountSpotify))
password_element$sendKeysToElement(list(passwordSpotify))
button_element$clickElement()
# + accept cookies...

listenedTracks <- data.frame()
# LOOPING dates
for (i in c(1:length(unProcessedDates))) {
  print(unProcessedDates[i])
  print("----------")
  # LOOPING all countries
  for (j in c(1:nrow(allPossibleCountries))) {
    print(allPossibleCountries$countryCode[j])
    url <- paste("https://charts.spotify.com/charts/view/regional-",allPossibleCountries$countryCode[j],"-daily/",unProcessedDates[i],sep="")
    # We generate a static version of this dynamic page
    remote_driver$navigate(url)
    
    # We need to delay until ensure page is loaded
    success <- FALSE  
    numTries <- 0
    while (!success & numTries <= 5) {
      Sys.sleep(1)  # delay to facilitate full load
      print("Waiting for page to fully load")      
      success <- tryCatch({
        remote_driver$findElement(
          using = 'xpath', 
          value = '//*[@id="__next"]/div/div/main/div[2]/div[3]/div/table/tbody/tr[1]/td[3]/div/div[1]/a')
        TRUE
      }, 
      warning = function(w) { FALSE },
      error = function(e) { FALSE },
      finally = { })
      numTries <- numTries + 1
    }
    
    # Now we are ready to process static page using rvest
    staticVersion <- remote_driver$getPageSource()[[1]] %>% read_html()   
    # We check page is loaded using a typical H3 appearing in failing loads
    pageLoaded <- staticVersion %>% 
      html_nodes(xpath = '//*[@id="__next"]/div/div/main/div[2]/div[2]/div/h3') %>% 
      is_empty()
    print(paste("URL:", url))
    print(paste("Loaded?:", pageLoaded))
    if (pageLoaded) { # Check no load error
      track <- NULL
      numStreams <- NULL
      # LOOPING top tracks
      for (k in c(1:numTopTracks)) {
        track[k] <- staticVersion %>% 
          html_nodes(xpath = 
                       paste('//*[@id="__next"]/div/div/main/div[2]/div[3]/div/table/tbody/tr[',k,']/td[3]/div/div[1]/a',sep="")) %>% 
          html_attr("href")
        track[k] <- str_remove(track[k],"https://open.spotify.com/track/")
        numStreams[k] <- staticVersion %>% 
          html_nodes(xpath = 
                       paste('//*[@id="__next"]/div/div/main/div[2]/div[3]/div/table/tbody/tr[',k,']/td[7]',sep="")) %>% html_text() 
        numStreams[k] <- as.numeric(gsub(",", "", numStreams[k]))
        print(paste("k =",k,"| track ->",track[k]))
        print(paste("k =",k,"| numStreams ->",numStreams[k]))  
      }  # ^^ TRACKS
      if (length(track[1:numTopTracks]) ==3) { # We'll append data only if actually extracted (page might have failed to load)
        tracksJustFound <-
          data.frame(trackId = track[1:numTopTracks],
                     numStreams = numStreams[1:numTopTracks],
                     date = unProcessedDates[i],
                     country = allPossibleCountries$country[j],
                     countryCode = allPossibleCountries$countryCode[j] )
        listenedTracks <- rbind(listenedTracks, tracksJustFound)
      }
    }
  }
}

head(listenedTracks)


# --------------------------------------------------------------------
# --------------------------------------------------------------------
# --------------------------------------------------------------------



# ================================
# STEP 3: Call Spotify API to obtain tracks features, and the consolidate to one value per date/country
# ================================
#
# Info on meaning of each feature:
# -Danceability: Numerical, danceability describes how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity. A value of 0.0 is least danceable and 1.0 is most danceable.
# -Tempo (BPM: Numerical, Overall estimated tempo of a track in beats per minute (BPM). In musical terminology, tempo is the speed or pace of a given piece and derives directly from the average beat duration.
# -Energy: Numerical, Energy is a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity. Typically, energetic tracks feel fast, loud, and noisy. For example, death metal has high energy, while a Bach prelude scores low on the scale. Perceptual features contributing to this attribute include dynamic range, perceived loudness, timbre, onset rate, and general entropy.
# (see description of other features here: https://rpubs.com/PeterDola/SpotifyTracks)

# calling API for tracks features
access_token <- get_spotify_access_token()
# There is a limit of 100 track in each query to the API, so we slice tracks in lots
recsToProc <- nrow(listenedTracks)  # total records (tracks) to process
maxRec <- 90  # limit of records per API (100, but just in case...)
nCalls <- 1 + floor((recsToProc-1)/maxRec) # calculate number of calls required
allTracksFeatures <- data.frame()  # we will accumulate results here

for (c in c(1:nCalls)) {
  tmpFromRecord <- 1+((c-1)*maxRec)
  tmpToRecord <- c*maxRec
  tmpToRecord <- ifelse(tmpToRecord <= recsToProc, tmpToRecord, recsToProc)
  print(paste("Obtaining features of tracks: ",tmpFromRecord, " to ", tmpToRecord))
  tmpTracksFeatures <-
    get_track_audio_features(
      listenedTracks[tmpFromRecord:tmpToRecord, 1],
      authorization = access_token)
  allTracksFeatures <- rbind(allTracksFeatures, tmpTracksFeatures)
}

listenedTracks  # source of tracks found
allTracksFeatures  # features extracted for those tracks
allTracksFeatures <- cbind(listenedTracks,allTracksFeatures) # we bind all columns
allTracksFeatures

# Finally, let's group by date/country/feature using weighted mean of the values of top track's features 
allTracksFeatures <- allTracksFeatures %>% 
  group_by(countryCode, country, date) %>% 
  summarize(
    danceability = weighted.mean(as.numeric(danceability), as.numeric(numStreams)), 
    energy = weighted.mean(as.numeric(energy), as.numeric(numStreams)), 
    tempo = weighted.mean(as.numeric(tempo), as.numeric(numStreams)), 
    key = weighted.mean(as.numeric(key), as.numeric(numStreams)), 
    loudness = weighted.mean(as.numeric(loudness), as.numeric(numStreams)),   
    speechiness = weighted.mean(as.numeric(speechiness), as.numeric(numStreams)),  
    acousticness = weighted.mean(as.numeric(acousticness), as.numeric(numStreams)),  
    instrumentalness = weighted.mean(as.numeric(instrumentalness), as.numeric(numStreams)),  
    liveness = weighted.mean(as.numeric(liveness), as.numeric(numStreams)),  
    valence = weighted.mean(as.numeric(valence), as.numeric(numStreams)))

# new data obtained
allTracksFeatures

# consolidate with historical data
allTracksFeatures <- rbind(allTracksFeatures, historicTracksFeatures)

unique(allTracksFeatures$date)
unique(allTracksFeatures$country)

# ================================
# SAVE
# ================================
# # Save dataset
# saveRDS(allTracksFeatures,"data/data_music_ts.rds")
# # Save also countries and codes for further consolidation with other data
# geo_music <- allTracksFeatures %>% group_by(countryCode, country) %>% summarise()
# geo_music
# saveRDS(geo_music,"data/geo_music.rds")

