


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
  paste(substring(tmpAvailableDates,7,10),"-",substring(tmpAvailableDates,1,2),"-",substring(tmpAvailableDates,4,5),sep="")
tmpAvailableCountryCodes
tmpAvailableCountries
tmpAvailableDates

track <- NULL
numStreams <- NULL
# vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
i <- 2 # country index
j <- 1 # date index

url_tracks <- paste("https://spotifycharts.com/regional/", tmpAvailableCountryCodes[i], "/", freqData, "/", tmpAvailableDates[j], sep="")
url_tracks
spotify_tracks <- read_html(url_tracks) %>% html_nodes(xpath='//*[@id="content"]/div/div/div/span/table/tbody/tr')
spotify_tracks
# vvvvvvvvvvvvvvvvvvvvv
k <- 1 # track index
track[k] <- 
  ((spotify_tracks[k] %>% html_nodes("td"))[1] %>% html_nodes("a") %>% html_children() %>% html_attrs())[[1]]
track[k] <- str_remove(track,"https://i.scdn.co/image/")
track[k]
numStreams[k] <- (spotify_tracks[k] %>% html_nodes("td"))[5] %>% html_text()
numStreams[k] <- as.numeric(str_replace_all(numStreams[k] ,",","")) # remove commas of thousands and convert to numeric
numStreams[k]
# ^^^^^^^^^^^^^^^^^^^^
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

track[1:numTopTracks]
numStreams[1:numTopTracks]






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