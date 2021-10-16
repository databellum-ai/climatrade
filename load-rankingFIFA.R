# Web visible: https://www.fifa.com/fifa-world-ranking/men
# Datos JSON: https://www.fifa.com/api/ranking-overview?locale=en&dateId=id13407

library(rvest)
library(jsonlite)

# Let's obtain available dates with their associated ids for URL
fifa_teams <- read_html("https://www.fifa.com/fifa-world-ranking/men")
jsonRawDates <- (fifa_teams %>% html_nodes("#__NEXT_DATA__") %>% html_text())
jsonRawDates <- fromJSON(jsonRawDates, flatten=TRUE)
ids_dates <- jsonRawDates$props$pageProps$pageData$ranking$dates
currentDate_id <- jsonRawDates$props$pageProps$pageData$ranking$selectedDate$id
currentDate <- jsonRawDates$props$pageProps$pageData$ranking$selectedDate$text
head(ids_dates)
currentDate_id
currentDate

# Let's obtain available dates with their associated ids for URL
tmpDateId <- "id13407"
jsonListTeams <- fromJSON(paste("https://www.fifa.com/api/ranking-overview?locale=en&dateId=",tmpDateId,sep=""), flatten=TRUE)
currentMonthRanking <- jsonListTeams$rankings
head(currentMonthRanking)
names(currentMonthRanking)
