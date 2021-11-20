# ====================================================
# CON RSelenium
# ====================================================

# https://thatdatatho.com/tutorial-web-scraping-rselenium/

library(RSelenium)
library(binman)  
list_versions("chromedriver")
# 
# #########################
# ### UPDATE 16/02/2020 ###
# #########################
# driver <- RSelenium::rsDriver(browser = "chrome",
#                               chromever =
#                                 system2(command = "wmic",
#                                         args = 'datafile where name="C:\\\\Program Files (x86)\\\\Google\\\\Chrome\\\\Application\\\\chrome.exe" get Version /value',
#                                         stdout = TRUE,
#                                         stderr = TRUE) %>%
#                                 stringr::str_extract(pattern = "(?<=Version=)\\d+\\.\\d+\\.\\d+\\.") %>%
#                                 magrittr::extract(!is.na(.)) %>%
#                                 stringr::str_replace_all(pattern = "\\.",
#                                                          replacement = "\\\\.") %>%
#                                 paste0("^",  .) %>%
#                                 stringr::str_subset(string =
#                                                       binman::list_versions(appname = "chromedriver") %>%
#                                                       dplyr::last()) %>%
#                                 as.numeric_version() %>%
#                                 max() %>%
#                                 as.character())
# remote_driver <- driver[["client"]] 

driver <- rsDriver(version = "latest", browser=c("chrome"), chromever = "96.0.4664.45")
remote_driver <- driver[["client"]]
remote_driver$navigate("https://www.latlong.net/convert-address-to-lat-long.html")

address_element <- remote_driver$findElement(using = 'class', value = 'width70')
address_element$sendKeysToElement(list("Lombard Street, San Francisco"))
button_element <- remote_driver$findElement(using = 'class', value = "button")
button_element$clickElement()
out <- remote_driver$findElement(using = "class", value="coordinatetxt")
lat_long <- out$getElementText()

street_names <- c("Lombard Street, San Francisco", 
                  "Santa Monica Boulevard", 
                  "Bourbon Street, New Orleans", 
                  "Fifth Avenue, New York", 
                  "Richards Street, Vancouver")

get_lat_lon <- function(street_names) {
  remote_driver$navigate("https://www.latlong.net/convert-address-to-lat-long.html")
  final <- c()
  for(i in 1:length(street_names)) {
    
    remote_driver$refresh()
    Sys.sleep(1)
    
    address_element <- remote_driver$findElement(using = 'class', value = 'width70')
    
    address_element$sendKeysToElement(list(street_names[i]))
    button_element <- remote_driver$findElement(using = 'class', value = "button")
    
    button_element$clickElement()
    Sys.sleep(3)
    
    out <- remote_driver$findElement(using = "class", value = "coordinatetxt")
    output <- out$getElementText()
    final <- c(final, output)
    
  }
  
  return(final)
}
vector_out <- get_lat_lon(street_names)

data.frame(street_names, purrr::flatten_chr(vector_out)) %>%
  dplyr::mutate(., vector_out = stringr::str_remove_all(vector_out, "\\(|\\)")) %>%
  tidyr::separate(., vector_out, into = c("latitude", "longitude"), sep = ",")

# ====================================

# https://levelup.gitconnected.com/web-scraping-with-r-part-2-dynamic-webpages-de620a161671


spotify_tracks <- read_html(remDr$getPageSource()[[1]])
# https://stackoverflow.com/questions/57370389/what-is-the-difference-between-rvesthtml-text-and-rseleniumgetpagesource?rq=1
spotify_tracks %>% html_nodes(xpath = '//*[@id="__next"]/div/div/main')
spotify_tracks

# rvest:
spotify_tracks <- read_html("https://charts.spotify.com/charts/view/regional-es-daily/2021-11-18")
tmp <- spotify_tracks %>%
  html_nodes(xpath = '//*[@id="__next"]/div/div/main/div[2]/div[3]/div') %>% html_attr("data-value")

# ====================================================
# CON rvest:
# ====================================================
spotify_tracks <- read_html("https://charts.spotify.com/charts/view/regional-bo-daily/2021-11-18")
tmp <- spotify_tracks %>% 
  html_nodes(xpath = '//*[@id="__next"]/div/div/main/div[2]/div[3]/div/table/tbody/tr[1]/td[7]') %>% 
  html_attr("data-value")
tmp

spotify_tracks <- read_html("https://charts.spotify.com/charts/view/regional-es-daily/2021-11-18")
tmp <- spotify_tracks %>% 
  html_nodes(xpath = '//*[@id="__next"]/div/div/main/div[2]/div[3]/div') %>% html_attr("data-value")
tmp


# ---------------------------------------------------
# ARCHIVO CSV CON LINK:
# ---------------------------------------------------
# https://charts-spotify-com-service.spotify.com/auth/v0/charts/regional-au-daily/2021-11-17
read.csv("https://spotifycharts.com/regional/global/daily/2020-07-29/download/regional-global-daily-2020-07-29.csv")
read_csv("https://spotifycharts.com/regional/global/daily/2020-07-29/download/regional-global-daily-2020-07-29.csv")
read.table("https://spotifycharts.com/regional/global/daily/2020-07-29/download/regional-global-daily-2020-07-29.csv")
library(data.table)
fread("https://spotifycharts.com/regional/global/daily/2020-07-29/download/regional-global-daily-2020-07-29.csv")
fread("https://spotifycharts.com/regional/global/daily/2020-07-29")


# ---------------------------------------------------
# CON LA API de Spotify:
# ---------------------------------------------------
# https://charts-spotify-com-service.spotify.com/auth/v0/charts/regional-be-daily/2021-11-07
# ===================================================
# https://medium.com/swlh/accessing-spotifys-api-using-r-1a8eef0507c
# https://developer.spotify.com/documentation/web-api/
library(httr)
#replace this with yours
clientID = 'xxxxxxxx'
secret = 'xxxxxxx'
response = POST(
  'https://accounts.spotify.com/api/token',
  accept_json(),
  authenticate(clientID, secret),
  body = list(grant_type = 'client_credentials'),
  encode = 'form',
  verbose()
)
mytoken = content(response)$access_token
HeaderValue = paste0('Bearer ', mytoken)

HeaderValue
mytoken

# ---------------
# MARKETS CON API:
response2 <- GET(url = 'https://api.spotify.com/v1/markets', add_headers(Authorization = HeaderValue))
tmp <- content(response2)$markets
tmp

# SEARCH CON API:
queryChain <- "search?q=artist:Julio%20Iglesias&type=album"
URI <- paste0('https://api.spotify.com/v1/', queryChain)
response <- GET(url = URI, add_headers(Authorization = HeaderValue))
tmp = content(response)
tmp



# ---------------------------------------------------
# CON LA API de Spotify (2):
# ---------------------------------------------------
artistID = "06HL4z0CvFAxyc27GXpf02"
URI = paste0('https://api.spotify.com/v1/artists/', artistID)
response2 = GET(url = URI, add_headers(Authorization = HeaderValue))
Artist = content(response2)
Artist

albumID = "1NAmidJlEaVgA3MpcPFYGq"
track_URI = paste0('https://api.spotify.com/v1/albums/', albumID,'/tracks')
track_response = GET(url = track_URI, add_headers(Authorization = HeaderValue))
tracks = content(track_response)

tracks

ntracks = length(tracks$items)
ntracks

tracks_list<-data.frame(
  name=character(ntracks),
  id=character(ntracks),
  artist=character(ntracks),
  disc_number=numeric(ntracks),
  track_number=numeric(ntracks),
  duration_ms=numeric(ntracks),
  stringsAsFactors=FALSE
)

tracks_list

for(i in 1:ntracks){
  tracks_list[i,]$id <- tracks$items[[i]]$id
  tracks_list[i,]$name <- tracks$items[[i]]$name
  tracks_list[i,]$artist <- tracks$items[[i]]$artists[[1]]$name
  tracks_list[i,]$disc_number <- tracks$items[[i]]$disc_number
  tracks_list[i,]$track_number <- tracks$items[[i]]$track_number
  tracks_list[i,]$duration_ms <- tracks$items[[i]]$duration_ms
}



# Get Additional Track Details
for(i in 1:nrow(tracks_list)){
  Sys.sleep(0.10)
  track_URI2 = paste0('https://api.spotify.com/v1/audio-features/',   
                      tracks_list$id[i])
  track_response2 = GET(url = track_URI2, 
                        add_headers(Authorization = HeaderValue))
  tracks2 = content(track_response2)
  
  tracks_list$key[i] <- tracks2$key
  tracks_list$mode[i] <- tracks2$mode
  tracks_list$time_signature[i] <- tracks2$time_signature
  tracks_list$acousticness[i] <- tracks2$acousticness
  tracks_list$danceability[i] <- tracks2$danceability
  tracks_list$energy[i] <- tracks2$energy
  tracks_list$instrumentalness[i] <- tracks2$instrumentalness
  tracks_list$liveliness[i] <- tracks2$liveness
  tracks_list$loudness[i] <- tracks2$loudness
  tracks_list$speechiness[i] <- tracks2$speechiness
  tracks_list$valence[i] <- tracks2$valence
  tracks_list$tempo[i] <- tracks2$tempo
}

tracks_list

# --------------
# PRUEBA PARA EXTRAER TODAS LAS TRACKS (VIENE DE:
# https://martijnvanvreeden.nl/collecting-spotify-data-with-r/)
auth_header <- httr::add_headers('Authorization'= paste('Bearer',mytoken))
recently_played <- httr::content(httr::GET('https://api.spotify.com/v1/me/player/recently-played',
                                           query=list(limit=50,time_range='long_range'),auth_header))
recently_played
# --------------
# TEST MÍO CON LO VISTO AL SCRAPEAR LA NUEVA PÁGINA:
countries_response <- GET(url = 'https://charts-spotify-com-service.spotify.com/auth/v0/charts/regional-be-daily/2021-11-07', 
                          add_headers(Authorization = HeaderValue))
countries = content(countries_response)
countries
# --------------





# =========================================

# https://martijnvanvreeden.nl/collecting-spotify-data-with-r/

library('httr')
library('jsonlite')
library('dplyr')
library('tidyr')
library('zoo')
library('purrr')
library('RCurl')


browseURL(paste0('https://accounts.spotify.com/authorize?client_id=',client_id,'&response_type=code&redirect_uri=',website_uri,'/&scope=user-read-recently-played'),browser = getOption("browser"), encodeIfNeeded = FALSE)

# https://charts-spotify-com-service.spotify.com/auth/v0/charts/regional-be-daily/2021-11-07


if(!file.exists(".spotify")){
  print("no file")
  
  #to get token FIRST TIME
  browseURL(paste0('https://accounts.spotify.com/authorize?client_id=',"xxxx",'&response_type=code&redirect_uri=',"https://databellum-ai.com/db/",'/&scope=user-read-recently-played'),browser = getOption("browser"), encodeIfNeeded = FALSE)
  
  #add new token
  user_code <- user_code_value
  
  
  #construct body of POST request FIRST TIME
  request_body <- list(grant_type='authorization_code',
                       code="prof02@yahoo.com",
                       redirect_uri="https://databellum-ai.com", #input your domain name
                       client_id = "xxxxx", #input your Spotify Client ID
                       client_secret = "xxxx") #input your Spotify Client Secret
  
  #get user tokens FIRST TIME
  user_token <- httr::content(httr::POST('https://accounts.spotify.com/api/token',
                                         body=request_body,
                                         encode='form'))
  
  user_token$access_token -> token
  auth_header <- httr::add_headers('Authorization'= paste('Bearer',token))
  write(user_token$refresh_token, ".spotify")
  
}

if(file.exists(".spotify")) {
  print("we have file")
  
  #REFRESH
  scan(file = ".spotify", what= list(id="")) -> red
  as.character(red) -> refresh_code
  request_body_refresh <- list(grant_type='refresh_token',
                               refresh_token=refresh_code,
                               redirect_uri="databellum-ai.com",
                               client_id = "xxxx",
                               client_secret = "xxxxx")
  
  #get user tokens REFRESH
  user_token_refresh <- httr::content(httr::POST('https://accounts.spotify.com/api/token',
                                                 body=request_body_refresh,
                                                 encode='form'))
  user_token_refresh$access_token -> token
}





request_body

user_token <- httr::content(httr::POST('https://accounts.spotify.com/api/token',
                                       body=request_body,
                                       encode='form'))


httr::content(httr::GET('https://api.spotify.com/v1/me/player/recently-played',
                        query=list(limit=50,time_range='long_range'),auth_header))


