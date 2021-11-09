
url_tracks <- "https://spotifycharts.com/regional/es/daily/2018-09-04"
url_tracks <- "https://spotifycharts.com/regional/cy/daily/2018-09-04"
url_tracks <- "https://spotifycharts.com/regional/be/daily/2018-09-04"

spotify_tracks <- 
  tryCatch(
    read_html(url_tracks) %>% 
      html_nodes(xpath='//*[@id="content"]/div/div/div/span/table/tbody/tr'), 
    error = function(e){NA})

length(spotify_tracks)

readingError_404 <- ifelse(spotify_tracks[1] == "error", TRUE, FALSE)
readingError_Zero <- (length(spotify_tracks) == 0)

readingError_404
readingError_Zero

if ( length(spotify_tracks) >= 3 ) {
  print("Funciona")
}





