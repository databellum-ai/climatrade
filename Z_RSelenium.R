# https://thatdatatho.com/tutorial-web-scraping-rselenium/

library(RSelenium)
library(binman)  
list_versions("chromedriver")
driver <- rsDriver(version = "latest", browser=c("chrome"), chromever = "96.0.4664.45")
remote_driver <- driver[["client"]]

url <- "https://accounts.spotify.com/en/login?continue=https:%2F%2Fcharts.spotify.com%2Flogin"

remote_driver$navigate(url)

address_element <- remote_driver$findElement(using = 'class', value = 'width70')
address_element$sendKeysToElement(list("Lombard Street, San Francisco"))
button_element <- remote_driver$findElement(using = 'class', value = "button")
button_element$clickElement()
out <- remote_driver$findElement(using = "class", value="coordinatetxt")
lat_long <- out$getElementText()

# url <- "https://charts.spotify.com/charts/view/regional-global-daily/latest"