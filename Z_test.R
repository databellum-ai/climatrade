
spotify_tracks <- read_html("https://spotifycharts.com/regional/global/daily/latest")
tmpAvailableCountryCodes <- spotify_tracks %>% 
  html_nodes(xpath = '//*[@id="content"]/div/div/div/span/div[1]/div/div/div/div[1]/ul/li') %>% 
  html_attr("data-value")
spotify_tracks

read_html("https://charts-spotify-com-service.spotify.com/auth/v0/charts/regional-be-daily/2021-11-07")

library(rvest)
url = "https://charts-spotify-com-service.spotify.com/auth/v0/charts/regional-be-daily/2021-11-07"
download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
content <- read_html("scrapedpage.html")


library(rvest)
library(curl)
read_html(curl('https://charts-spotify-com-service.spotify.com/auth/v0/charts/regional-be-daily/2021-11-07', handle = curl::new_handle("useragent" = "Mozilla/5.0")))
