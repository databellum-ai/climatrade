# https://developer.spotify.com/documentation/web-api/reference/
# https://rdrr.io/cran/spotifyr/man/get_my_top_artists_or_tracks.html
# https://msmith7161.github.io/what-is-speechiness/

# PTE:
# -Separar por país
# -Nivel canción y sus características (BPM, etc.)

Sys.setenv(SPOTIFY_CLIENT_ID = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx')
source("clavesAPI_spotify.R")

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