# =======================
# FIFA Ranking extraction
# =======================

library(tidyverse)
library(rvest)
library(jsonlite)
library(lubridate)

# Let's obtain available dates with their associated ids for each URL "issue"
fifa_teams <- read_html("https://www.fifa.com/fifa-world-ranking/men")
jsonRawDates <- (fifa_teams %>% html_nodes("#__NEXT_DATA__") %>% html_text())
jsonRawDates <- fromJSON(jsonRawDates, flatten=TRUE)
tmpdates <- jsonRawDates$props$pageProps$pageData$ranking$dates
tmpdates$text <- str_replace(tmpdates$text, "Sept", "Sep")  # Month must be corrected to 3 letter for compatibility
currentDate_id <- jsonRawDates$props$pageProps$pageData$ranking$selectedDate$id
currentDate <- jsonRawDates$props$pageProps$pageData$ranking$selectedDate$text
currentDate <- as.Date(parse_date_time(currentDate, orders = "d b Y", locale = "us"))
ids_datesAvailable <- tmpdates$id
datesAvailable <- tmpdates$text

# last issue/date
currentDate_id
currentDate

# now we get ranking for each historical issue and accumulate it in a dataframe
for(i in 1:length(ids_datesAvailable)) {
  print(paste(
    "Date:",datesAvailable[i],"processing",i,"of",length(ids_datesAvailable),"dates"))
  pageAtDate <- paste("https://www.fifa.com/api/ranking-overview?locale=en&dateId=",ids_datesAvailable[i],sep="")
  # Let's obtain available dates with their associated ids for URL
  jsonListTeams <- fromJSON(pageAtDate, flatten=TRUE)
  currentMonthRanking <- jsonListTeams$rankings
  currentMonthRanking <- currentMonthRanking %>% 
    mutate(Date = as.Date(parse_date_time(datesAvailable[i], orders = "d b Y", locale = "us")), 
           Rank = rankingItem.rank, 
           CountryName = rankingItem.name, 
           Points = rankingItem.totalPoints, 
           CountryCode = rankingItem.countryCode, 
           idRegion = tag.id, 
           lastIssue = currentDate_id, 
           lastIssueDate = currentDate) %>% 
    select(Date, Rank, CountryName, Points, CountryCode, idRegion, lastIssue, lastIssueDate)
  if(i==1) {
    historicalRankingFIFA <- currentMonthRanking
  } else {
    historicalRankingFIFA <- rbind(currentMonthRanking, historicalRankingFIFA)
  }
  }

head(historicalRankingFIFA)

# We extract countries and regions for standardization in further steps
geo_FIFA <- historicalRankingFIFA %>% group_by(CountryCode) %>% summarise(CountryName = first(CountryName), Region = first(idRegion))
# Prepare data per date and country
historicalRankingFIFA <- historicalRankingFIFA %>% select(Date, CountryCode, Rank)


# Save to RDS
saveRDS(as_tibble(historicalRankingFIFA), "data/data_FIFA_ts.rds")
saveRDS(as_tibble(geo_FIFA), "data/geo_FIFA.rds")

