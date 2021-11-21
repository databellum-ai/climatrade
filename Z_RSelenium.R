# PTE: pasar login
# PTE: leer países
# PTE: lanzar por fechas
# PTE: integrar con original

# https://thatdatatho.com/tutorial-web-scraping-rselenium/

library(RSelenium)
library(binman)
# system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE) 
list_versions("chromedriver")
driver <- rsDriver(version = "latest", browser=c("chrome"), chromever = "96.0.4664.45")
remote_driver <- driver[["client"]]


dateToExtract <- "2021-11-19"
url <- paste("https://charts.spotify.com/charts/view/regional-global-daily/",dateToExtract,sep="")
remote_driver$navigate(url)
staticVersion <- remote_driver$getPageSource()[[1]]
spotify_tracks <- read_html(staticVersion) # rvest
numTopTracks <- 3
track <- NULL
numStreams <- NULL
listenedTracks <- data.frame()
# LOOPING top tracks
for (k in c(1:numTopTracks)) {
  track[k] <- spotify_tracks %>% 
    html_nodes(xpath = paste('//*[@id="__next"]/div/div/main/div[2]/div[3]/div/table/tbody/tr[',k,']/td[3]/div/div[1]/a',sep="")) %>% html_attr("href")
  track[k] <- str_remove(track[k],"https://open.spotify.com/track/")
  numStreams[k] <- spotify_tracks %>% 
    html_nodes(xpath = paste('//*[@id="__next"]/div/div/main/div[2]/div[3]/div/table/tbody/tr[',k,']/td[7]',sep="")) %>% html_text() 
  numStreams[k] <- as.numeric(gsub(",", "", numStreams[k]))
}  # ^^ TRACKS
tracksJustFound <-
  data.frame(trackId = track[1:numTopTracks],
             numStreams = numStreams[1:numTopTracks],
             date = NA,
             country = NA,
             countryCode = NA)
listenedTracks <- rbind(listenedTracks, tracksJustFound)
head(listenedTracks)



address_element <- remote_driver$findElement(using = 'class', value = 'width70')
address_element$sendKeysToElement(list("Lombard Street, San Francisco"))
button_element <- remote_driver$findElement(using = 'class', value = "button")
button_element$clickElement()
out <- remote_driver$findElement(using = "class", value="coordinatetxt")
lat_long <- out$getElementText()

# url <- "https://charts.spotify.com/charts/view/regional-global-daily/latest"