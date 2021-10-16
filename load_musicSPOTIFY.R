# https://developer.spotify.com/documentation/web-api/reference/
# https://rdrr.io/cran/spotifyr/man/get_my_top_artists_or_tracks.html
# https://msmith7161.github.io/what-is-speechiness/
#
# For certain functions and applications, you’ll need to log in as a Spotify user. To do this, your Spotify Developer application needs to have a callback url. You can set this to whatever you want that will work with your application, but a good default option is http://localhost:1410/ (see image below). For more information on authorization, visit the offical Spotify Developer Guide. (https://www.rcharlie.com/spotifyr/)
install.packages("spotifyr")

library(spotifyr)

Sys.setenv(SPOTIFY_CLIENT_ID = '7f00e459e82b4d85a3a3b5b29e34879a')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'bdbd6c4fa0c74bb39e5a82a68989e0fe')
access_token <- get_spotify_access_token()


# https://stackoverflow.com/questions/42130591/most-popular-tracks-list-using-the-spotify-api
tmpCategories <- get_categories()$id

get_category_playlists(
  category_id = "toplists",
  country = NULL,
  limit = 50
)





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