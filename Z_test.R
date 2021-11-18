# https://charts-spotify-com-service.spotify.com/auth/v0/charts/regional-be-daily/2021-11-07


# ===================================================
# https://medium.com/swlh/accessing-spotifys-api-using-r-1a8eef0507c
# https://developer.spotify.com/documentation/web-api/

library(httr)
#replace this with yours
clientID = '7f00e459e82b4d85a3a3b5b29e34879a'
secret = 'bdbd6c4fa0c74bb39e5a82a68989e0fe'
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
  browseURL(paste0('https://accounts.spotify.com/authorize?client_id=',"7f00e459e82b4d85a3a3b5b29e34879a",'&response_type=code&redirect_uri=',"https://databellum-ai.com/db/",'/&scope=user-read-recently-played'),browser = getOption("browser"), encodeIfNeeded = FALSE)
  
  #add new token
  user_code <- user_code_value
  
  
  #construct body of POST request FIRST TIME
  request_body <- list(grant_type='authorization_code',
                       code="prof02@yahoo.com",
                       redirect_uri="https://databellum-ai.com", #input your domain name
                       client_id = "05a635286ba1439587b6e18d7875633c", #input your Spotify Client ID
                       client_secret = "909130c5b2714ebeb3b7d1889cab2ab8") #input your Spotify Client Secret
  
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
                               client_id = "7f00e459e82b4d85a3a3b5b29e34879a",
                               client_secret = "bdbd6c4fa0c74bb39e5a82a68989e0fe")
  
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


