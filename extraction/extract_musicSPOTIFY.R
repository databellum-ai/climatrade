# PTE: carga de volumen
# PTE: extraer .RDS "geo" con los países y su nombre



# ================================
# STEP 1: Determine countries and dates we need to collect
# ================================

# We extract available countries and dates depending on frequency chosen (daily/weekly)  
if (freqData == "daily") {
  # Extract country codes
  spotify_tracks <- read_html("https://spotifycharts.com/regional/global/daily/latest")
  tmpAvailableCountryCodes <- spotify_tracks %>% 
    html_nodes(xpath = '//*[@id="content"]/div/div/div/span/div[1]/div/div/div/div[1]/ul/li') %>% 
    html_attr("data-value")
  # Extract country names
  tmpAvailableCountries <- spotify_tracks %>% 
    html_nodes(xpath = '//*[@id="content"]/div/div/div/span/div[1]/div/div/div/div[1]/ul/li') %>% 
    html_text()
  # Extraction of dates available when DAILY
  tmpAvailableDates <- spotify_tracks %>% 
    html_nodes(xpath = '//*[@id="content"]/div/div/div/span/div[1]/div/div/div/div[3]/ul') %>% 
    html_children() %>% html_text()  
}
if (freqData == "weekly") {
  # Extract country codes
  spotify_tracks <- read_html("https://spotifycharts.com/regional/global/weekly/latest")
  tmpAvailableCountryCodes <- spotify_tracks %>% 
    html_nodes(xpath = '//*[@id="content"]/div/div/div/span/div[1]/div/div/div/div[1]/ul/li') %>% 
    html_attr("data-value")
  # Extract country names
  tmpAvailableCountries <- spotify_tracks %>% 
    html_nodes(xpath = '//*[@id="content"]/div/div/div/span/div[1]/div/div/div/div[1]/ul/li') %>% 
    html_text()
  # Extract of dates available when WEEKLY
  tmpAvailableDates <- spotify_tracks %>% 
    html_nodes(xpath = '//*[@id="content"]/div/div/div/span/div[1]/div/div/div/div[3]/ul') %>% 
    html_children() %>% html_text()
}
tmpAvailableDates <-
  paste(substring(tmpAvailableDates,7,10),"-",substring(tmpAvailableDates,1,2),"-",substring(tmpAvailableDates,4,5),sep="") # convert date format
tmpAvailableCountryCodes
tmpAvailableCountries
tmpAvailableDates


# read already available data to ensure claculation only of delta. At the end we'll consolidate
if (file.exists("data/data_music_ts.rds")) {
  historicTracksFeatures <- readRDS("data/data_music_ts.rds")
  # get a list of dates appearing in the website but not in our historic record and use it for further process
  existingDates <- historicTracksFeatures %>% group_by(date) %>% summarize()
  existingDates
  unProcessedDates <- tmpAvailableDates[!(tmpAvailableDates %in% existingDates$date)]
  unProcessedDates
} else {
  # in case there is no history of data, we must process all available (on the web) dates
  historicTracksFeatures <- data.frame()
  unProcessedDates <- tmpAvailableDates
}
# to eventually process only a limited number of recent dates:
unProcessedDates <- unProcessedDates[1:30]

unProcessedDates



# ================================
# STEP 2: Scrape Spotify web site and obtain list of top tracks per date/country, collecting also number of streams
# ================================

track <- NULL
numStreams <- NULL
listenedTracks <- data.frame()
# LOOPING dates
for (i in c(1:length(unProcessedDates))) {
  print(paste("Date: ",unProcessedDates[i],sep=""))
  # LOOPING all countries
  for (j in c(1:length(tmpAvailableCountries))) {
  # for (j in c(17,42)) {
    print(tmpAvailableCountries[j])
    url_tracks <-
      paste("https://spotifycharts.com/regional/", tmpAvailableCountryCodes[j], "/", freqData, "/", unProcessedDates[i], sep="")
    # we obtain NA is case requested page does not exist or returns an error
    spotify_tracks <- tryCatch(read_html(url_tracks) %>%
                                 html_nodes(xpath='//*[@id="content"]/div/div/div/span/table/tbody/tr'), error = function(e){NA})
    if (!(is.na(spotify_tracks))) {
      # LOOPING top tracks
      for (k in c(1:numTopTracks)) {
        # track index
        track[k] <-
          (((spotify_tracks[k] %>% html_nodes("td"))[1] %>% html_nodes("a") %>% html_attrs())[[1]])[1]
        track[k] <-
          str_remove(track[k],"https://open.spotify.com/track/")
        # Scrape number of streams per track
        numStreams[k] <- (spotify_tracks[k] %>% html_nodes("td"))[5] %>% html_text()
        numStreams[k] <- as.numeric(gsub(",", "", numStreams[k]))
      }  # ^^ TRACKS
      tracksFoundOnDate <-
        data.frame(trackId = track[1:numTopTracks],
                   numStreams = numStreams[1:numTopTracks],
                   date = unProcessedDates[i],
                   country = tmpAvailableCountries[j],
                   countryCode = tmpAvailableCountryCodes[j])
      listenedTracks <- rbind(listenedTracks, tracksFoundOnDate)
    } # END IF to ensure page is reachable
  } # ^^ DATES
} # ^^ COUNTRIES

listenedTracks


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
  group_by(country, date) %>% 
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

saveRDS(allTracksFeatures,"data/data_music_ts.rds")



