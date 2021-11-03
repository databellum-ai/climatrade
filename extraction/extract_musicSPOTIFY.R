# PTE: decidir qué fechas bajo cada vez (+ carga histórico incremental)
# PTE: convertir fecha a "date" en la salida
# PTE: bajar todoslos países
# PTE: extraer .RDS "geo" con los países y su nombre
# PTE: reducir a 100 track cada llamada a la API



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



# ================================
# STEP 2: Scrape Spotify web site and obtain list of top tracks per date/country, collecting also number of streams
# ================================

track <- NULL
numStreams <- NULL
listenedTracks <- data.frame()
# LOOPING countries
# for (i in c(1:length(tmpAvailableDates))) {
for (i in c(1:5)) {
  print(paste("Date: ",tmpAvailableDates[i],sep=""))
  # LOOPING all dates
  # for (j in c(1:length(tmpAvailableDates))) {
  for (j in c(1:2)) {
    print(tmpAvailableCountries[j])
    url_tracks <- 
      paste("https://spotifycharts.com/regional/", tmpAvailableCountryCodes[j], "/", freqData, "/", tmpAvailableDates[i], sep="")
    spotify_tracks <- read_html(url_tracks) %>% 
      html_nodes(xpath='//*[@id="content"]/div/div/div/span/table/tbody/tr')
    spotify_tracks
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
                 date = tmpAvailableDates[i], 
                 country = tmpAvailableCountries[j], 
                 countryCode = tmpAvailableCountryCodes[j])
    listenedTracks <- rbind(listenedTracks, tracksFoundOnDate)
  } # ^^ DATES
} # ^^ COUNTRIES
listenedTracks


# ================================
# STEP 3: Call Spotify API to obtain tracks features, and the consolidate to one value per date/country
# ================================
#
# Info on meaning of each feature: https://rpubs.com/PeterDola/SpotifyTracks +:
# -Danceability: Numerical, danceability describes how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity. A value of 0.0 is least danceable and 1.0 is most danceable.
# -Tempo (BPM: Numerical, Overall estimated tempo of a track in beats per minute (BPM). In musical terminology, tempo is the speed or pace of a given piece and derives directly from the average beat duration.
# -Energy: Numerical, Energy is a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity. Typically, energetic tracks feel fast, loud, and noisy. For example, death metal has high energy, while a Bach prelude scores low on the scale. Perceptual features contributing to this attribute include dynamic range, perceived loudness, timbre, onset rate, and general entropy.

# calling API for tracks features
access_token <- get_spotify_access_token()
tmpTracksFeatures <- 
  get_track_audio_features(
    listenedTracks$trackId, 
    authorization = access_token)
tmpTracksFeatures <- tmpTracksFeatures %>% 
  select("danceability","energy","tempo")




recsToProc <- nrow(listenedTracks)  # records to process
recsToProc <- 30
maxRec <- 8  # limit of records per API 
nCalls <- trunc(0.5 + recsToProc/maxRec) # calculate number of call required

for (c in c(1:nCalls)) {
  tmpFromRecord <- 1+((c-1)*maxRec)
  tmpToRecord  <- c*maxRec
  if (tmpToRecord ...)
  print(paste("Call:",tmpFromRecord, tmpToRecord))
}




listenedTracks
tmpTracksFeatures
cbind(listenedTracks,tmpTracksFeatures)

# ================================
# SAVE
# ================================

