# PTE: leer datos de la TRACK de SPOTIFY
# PTE: decidir qué fechas bajo cada vez (+ carga histórico?)
# PTE: convertir fecha a "date" en la salida
# PTE: bajar todoslos países (?)
# PTE: extraer .RDS "geo" con los países y su nombre
# PTE: comentarizar código


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


track <- NULL
numStreams <- NULL
allTracks <- data.frame()
# LOOPING countries
# for (i in c(1:length(tmpAvailableDates))) {
for (i in c(1:5)) {
  print(paste("Date: ",tmpAvailableDates[i],sep=""))
  # LOOPING all dates
  # for (j in c(1:length(tmpAvailableDates))) {
  for (j in c(1:2)) {
    print(tmpAvailableCountries[j])
    url_tracks <- 
      paste("https://spotifycharts.com/regional/", tmpAvailableCountryCodes[j], "/", freqData, "/", tmpAvailableDates[j], sep="")
    spotify_tracks <- read_html(url_tracks) %>% 
      html_nodes(xpath='//*[@id="content"]/div/div/div/span/table/tbody/tr')
    spotify_tracks
    # LOOPING top tracks
    for (k in c(1:numTopTracks)) {
      # track index
      track[k] <- 
        ((spotify_tracks[k] %>% html_nodes("td"))[1] %>% html_nodes("a") %>% html_children() %>% html_attrs())[[1]]
      track[k] <- 
        str_remove(track[k],"https://i.scdn.co/image/")
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
    allTracks <- rbind(allTracks, tracksFoundOnDate)
  } # ^^ DATES
} # ^^ COUNTRIES
allTracks

# ------------------------------------------------
tmpTracksIds <- c("3bMdWvDfVZwwEkPVSmIqt1", "49Zt8N0m0RLLFzCELwV7yG", "1HEHQ6UgRvbJ4byX7JIuF9", "2xdVantHk5zayugYswViOh", "3sAsM39lExnrDY9R3NCNh2", "0CB8UyGmMO0G7nLEmjmCyn", "5RaquHTS3o2qz44TVyLzKg", "5r6ADOcMFbdo5VSgTmjXfw", "4tX1L7YYwkXeQAuQJ4Yhl2", "6oFnxJJDkrLPvF2X6cEI5D", "1UWbBFaZNksb3AmgldkprR", "6SnD5dJk06mhNQvvF270cw", "7CSrmKCRz7g9Z4GdKg3Asn", "4HgxCQLqNcd8s6lyoR4aag", "4SDscXJTjdF2YgSkYADyU0", "36HznXQNGoen7dwq4Vv6nP", "7rLB8H9GIzlxgXxXVqQltR", "1nht40JT9sTCtXxrpPi5Gm", "5dIgCsaxQnexMVpHXRHWpu", "1853Cr5Cf8UjmPAWhidVn9", "5K2rrGyZ3ggTF6y0n3lM5v", "0WGmmWaQezfhLxrH0vmuOf", "48NLsNOGOQbKEWxFXcihUM", "7tljncoprKWXchoN4KUziP", "6ZCfm60qPoAGTmV6mxBDJ7", "1208J1WMoVXWdyfEgGM8OT", "2iBKjbODc0lVr39itL51Ty", "6UNXLI8GSXkXo53Oin16uo", "1KePwwly0kr3TG2ankaXwd", "3lFWdUEDo0ZFK2bzDBqHfM")

access_token <- get_spotify_access_token()

tmpTracksFeatures <- get_track_audio_features(
  tmpTracksIds, 
  authorization = access_token)
tmpTracksFeatures %>% select("id","duration_ms",1:11)
names(tmpTracksFeatures)








tmpTracksData <- 
  get_tracks(
    ids = tmpTracksIds,
    market = NULL,
    authorization = access_token,
    include_meta_info = TRUE
  )
tmpTracksData$tracks %>% select("id", -"available_markets", "duration_ms", -"href", -"preview_url", -"uri", -"external_urls.spotify", "external_ids.isrc")    






# https://rpubs.com/jorgelopez141/fifaWebsite


# =============================
# =============================


# https://developer.spotify.com/documentation/web-api/reference/
# https://rdrr.io/cran/spotifyr/man/get_my_top_artists_or_tracks.html
# https://msmith7161.github.io/what-is-speechiness/

# PTE:
# -Separar por país
# -Nivel canción y sus características (BPM, etc.)

access_token <- get_spotify_access_token()

# https://stackoverflow.com/questions/42130591/most-popular-tracks-list-using-the-spotify-api
tmpCategories <- get_categories()$id

tmpPlaylists <- get_category_playlists(
  category_id = "toplists",
  country = NULL,
  limit = 50
)
tmpPlaylists <- tmpPlaylists %>% 
  filter(id =="37i9dQZF1DXcBWIGoYBM5M" | 
           id =="37i9dQZEVXbMDoHDwVN2tF" | 
           id =="37i9dQZEVXbLiRSasKsNU9") %>% 
  select(id, name, description)

tmpPlaylists
nrow(tmpPlaylists)





# -----------------------------------------------------------------------

beatles <- get_artist_audio_features('the beatles')
class(beatles)
names(beatles)
head(beatles)
nrow(beatles)

tracks <- get_playlist_tracks(my_plists2)
features <- get_track_audio_features(tracks)
# Do a left_join to join the two tables (playlist tracks and track features) by the “track_uri” column.
tracks2 <- tracks%>%
  left_join(features, by="track_uri")