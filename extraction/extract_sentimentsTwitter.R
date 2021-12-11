# PTE: función de limpeza previa de los textos
# PTE: fechas: valorar/PROBAR con Twitter problema de sólo 10 días hacia atrás...COMPROBAR NIVEL ACTUAL EN TWITTER ("Elevated?"): https://developer.twitter.com/en/docs/twitter-api/getting-started/about-twitter-api#Access

numTweetsExtract <- 50
sentimentTerms <- list("miscelaneous" = c("from:potus", "Amazon", "from:sanchezcastejon+españa"), 
                       "football" = c("real madrid", "psg", "messi"))
library(tidyverse)
library(dplyr)
library(ROAuth)
library(twitteR)


# connect to twitter API from the twitter package
setup_twitter_oauth(twitter_api_key, twitter_api_secret, twitter_access_token, twitter_access_token_secret)


# FUNCTIONS ============
cleanTexts <- function(dfTexts) {
  # sentence = gsub('https://','',sentence) # removes https://
  # sentence = gsub('http://','',sentence) # removes http://
  # sentence = gsub('[^[:graph:]]', ' ',sentence) ## removes graphic characters like emoticons
  # sentence = str_replace_all(sentence,"[^[:graph:]]", " ")
  # 
  # sentence = gsub("[[:punct:]]", "", sentence) # removes punctuation (and @)
  # sentence = gsub("[[:cntrl:]]", "", sentence) # removes control characters
  # sentence = gsub("\\d+", "", sentence) # removes numbers
  # sentence = tolower(sentence)
  
  # # remove repeated and clean "..."
  # tweets_found.nodups.df <- distinct(tweets_found.df, text, .keep_all = TRUE)
  # tweets_found.nodups.df$text <- gsub("… ", "", tweets_found.nodups.df$text)
  # # change the feature created to Date
  # tweets_found.nodups.df <- tweets_found.nodups.df %>% 
  #   mutate(created = as.Date(tweets_found.nodups.df$created)) 
  # names(tweets_found.nodups.df)[names(tweets_found.nodups.df) == "created"] <- "date"
  # # transform from datetime to date format
  # tweets_text <- lapply(tweets_found, function(x) x$getText())
  # # delete the (retweets)
  # tweets_unique <- unique(tweets_text) c
  dfTexts
}
extractTermTweets <- function(term, KAM) {
  tweetsList <- searchTwitter(term, resultType="recent", n=numTweetsExtract)
  print(paste0("RetriEving tweets related with ",KAM,"::",term))
  if (length(tweetsList) >0) {
    twListToDF(tweetsList) %>% 
      cleanTexts() %>% 
      mutate(
        score=get_sentiment(text, method="syuzhet"), 
        date = as.Date(created), 
        term = term, KAM=KAM) %>% 
      select(date, score, term, KAM)    
  } else {
    data.frame()
  }

}
analyzeListTerms <- function(listTerms, listName) {
  lapply(listTerms, function(j) extractTermTweets(j, listName) %>% 
           bind_rows())
}

# =============================================
# TREAT TEXT AND CALCULATE SENTIMENT
# =============================================
allTexts.df <- 
  lapply(seq_along(sentimentTerms), function(i) analyzeListTerms(sentimentTerms[[i]],names(sentimentTerms)[[i]])) %>% 
  bind_rows() %>% 
  group_by(date, term, KAM) %>% 
  summarise(score=mean(score))


# =============================================
# READ THE TIDY VERSION AS BASE FOR INCREMENTAL ADDITION
# =============================================
historicSentiment <- readRDS("data/data_twitterSentiment_tidy.rds")
historicSentiment
newTotalData <- rbind(historicSentiment,allTexts.df)
newTotalData
# =============================================
# SAVE A TIDY VERSION FOR INCREMENTAL ADDITION
# =============================================
saveRDS(newTotalData,"data/data_twitterSentiment_tidy.rds")
# =============================================
# SAVE A WORKING TIME SERIES VERSION
# =============================================
newTotalData_ts <- newTotalData %>% 
  group_by(date, KAM) %>% 
  summarise(score = mean(score)) %>% 
  spread(key=KAM, value=score, fill=NA)
saveRDS(newTotalData_ts,"data/data_twitterSentiment_ts.rds")

newTotalData_ts







