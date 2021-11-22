# PTE: pasar login
# PTE: leer países
# PTE: lanzar por fechas
# PTE: integrar con original

# https://thatdatatho.com/tutorial-web-scraping-rselenium/

library(tidyverse)
library(rvest)

library(RSelenium)
library(binman)

#Shut Down Client and Server
# remote_driver$close()
# driver$server$stop()
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



# ----------------------------------

# <div data-testid="region-filter" class="RegionSelect__OverviewFilter-ibo3np-0 ilDAmP"><span class="Trigger-sc-18ecbf7-0 cGQwPM Trigger-uvfeg5-0 jQtrTN RegionSelect__StyledDropdownTrigger-ibo3np-1 iBVLW"><div class="Container-s1cmq-0 fQlZAH"><button aria-haspopup="listbox" id="dropdown-toggle" aria-labelledby="dropdown-label dropdown-toggle" aria-expanded="false" class="Button-sc-18tio8-0 jPCnoE">Global</button><svg role="img" focusable="false" height="16" width="16" viewBox="0 0 24 24" aria-hidden="true" class="Svg-sc-1usfroi-0 jhoCoB Arrow-sc-62daq7-0 dKKkZe"><polyline points="20 8 12 17 4 8" fill="none" stroke="#181818"></polyline></svg></div></span></div>

# <span class="Trigger-sc-18ecbf7-0 cGQwPM Trigger-uvfeg5-0 jQtrTN RegionSelect__StyledDropdownTrigger-ibo3np-1 iBVLW"><div class="Container-s1cmq-0 fQlZAH"><button aria-haspopup="listbox" id="dropdown-toggle" aria-labelledby="dropdown-label dropdown-toggle" aria-expanded="false" class="Button-sc-18tio8-0 jPCnoE">Australia</button><svg role="img" focusable="false" height="16" width="16" viewBox="0 0 24 24" aria-hidden="true" class="Svg-sc-1usfroi-0 jhoCoB Arrow-sc-62daq7-0 dKKkZe"><polyline points="20 8 12 17 4 8" fill="none" stroke="#181818"></polyline></svg></div></span>


test <- remote_driver$findElement(using = 'xpath', value = '//*[@id="dropdown-toggle"]')
test <- remote_driver$findElement(using = 'class', value = 'Container-s1cmq-0')

test <- remote_driver$findElement(using = 'xpath', value = '//*[@id="__next"]/div/div/main/div[2]/div[1]/header/div/div[2]/div/span/div')
view(test$getElementText())


test <- remote_driver$findElement(using = 'xpath', value = '//*[@id="__next"]/div/div/main/div[2]/div[1]/header/div/div[2]/div/span')
test$clickElement()
test$getActiveElement()

element <- unlist(remote_driver$findElement("class name", "Button-sc-18tio8-0")$getElementAttribute('class'))
length(element)


test <- remote_driver$findElement("data-testid", "region-filter")
test

buttonElem <- remote_driver$findElement("class name", "Button-sc-18tio8-0")
view(buttonElem$getElementText())
buttonElem$clickElement()

# -----------------------------------------


address_element <- remote_driver$findElement(using = 'class', value = 'width70')
address_element$sendKeysToElement(list("Lombard Street, San Francisco"))
button_element <- remote_driver$findElement(using = 'class', value = "button")
button_element$clickElement()
out <- remote_driver$findElement(using = "class", value="coordinatetxt")
lat_long <- out$getElementText()

# url <- "https://charts.spotify.com/charts/view/regional-global-daily/latest"