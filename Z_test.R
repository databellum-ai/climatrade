print(tmpAvailableCountries[2])
url_tracks <- 
  paste("https://spotifycharts.com/regional/", tmpAvailableCountryCodes[2], "/", freqData, "/", tmpAvailableDates[1], sep="")
url_tracks
spotify_tracks <- read_html(url_tracks) %>% 
  html_nodes(xpath='//*[@id="content"]/div/div/div/span/table/tbody/tr')
spotify_tracks


track[k] <- 
  ((spotify_tracks[k] %>% html_nodes("td"))[1] %>% html_nodes("a") %>% html_children() %>% html_attrs())[[1]]


read_html(url_tracks) %>% 
  html_nodes(xpath='//*[@id="content"]/div/div/div/span/table/tbody/tr[1]/td[1]/a') %>% html_children()

tmp[1]


tmp <- (((spotify_tracks[k] %>% html_nodes("td"))[1] %>% html_nodes("a") %>% html_attrs())[[1]])[1]


tmp[1]
class(tmp)
length(tmp)
