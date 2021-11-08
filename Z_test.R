
url_tracks <- "https://spotifycharts.com/regional/es/daily/2019-01-02"
url_tracks <- "https://spotifycharts.com/regional/cy/daily/2019-01-02"
url_tracks <- "https://spotifycharts.com/regional/do/daily/2019-01-02"

spotify_tracks <- 
  tryCatch(
    read_html(url_tracks) %>% 
      html_nodes(xpath='//*[@id="content"]/div/div/div/span/table/tbody/tr'), 
    error = function(e){"error"})

length(spotify_tracks)

readingError_404 <- ifelse(spotify_tracks[1] == "error", TRUE, FALSE)
readingError_Zero <- (length(spotify_tracks) == 0)

readingError_404
readingError_Zero

if ( !(readingError_404) & !(readingError_Zero) ) {
  print("Funciona")
}


