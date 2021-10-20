# Web visible: https://www.fifa.com/fifa-world-ranking/men
# Datos JSON: https://www.fifa.com/api/ranking-overview?locale=en&dateId=id13407



# Let's obtain available dates with their associated ids for each URL "issue"
fifa_teams <- read_html("https://www.fifa.com/fifa-world-ranking/men")
jsonRawDates <- (fifa_teams %>% html_nodes("#__NEXT_DATA__") %>% html_text())
jsonRawDates <- fromJSON(jsonRawDates, flatten=TRUE)
tmpdates <- jsonRawDates$props$pageProps$pageData$ranking$dates
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
  pageAtDate <- paste("https://www.fifa.com/api/ranking-overview?locale=en&dateId=",ids_datesAvailable[i],sep="")
  # Let's obtain available dates with their associated ids for URL
  jsonListTeams <- fromJSON(pageAtDate, flatten=TRUE)
  currentMonthRanking <- jsonListTeams$rankings
  currentMonthRanking <- currentMonthRanking %>% 
    mutate(issueDate = as.Date(parse_date_time(datesAvailable[i], orders = "d b Y", locale = "us")), 
           Rank = rankingItem.rank, 
           CountryName = rankingItem.name, 
           Points = rankingItem.totalPoints, 
           CountryCode = rankingItem.countryCode, 
           idRegion = tag.id, 
           lastIssue = currentDate_id, 
           lastIssueDate = currentDate) %>% 
    select(issueDate, Rank, CountryName, Points, CountryCode, idRegion, lastIssue, lastIssueDate)
  if(i==1) {
    historicalRankingFIFA <- currentMonthRanking
  } else {
    historicalRankingFIFA <- rbind(currentMonthRanking, historicalRankingFIFA)
  }
  }

head(historicalRankingFIFA)

# Save to RDS
saveRDS(historicalRankingFIFA, "./data/rankingFIFA.rds")
