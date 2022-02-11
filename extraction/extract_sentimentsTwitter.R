

# =============================================
# EXTRACT TWEETS AND ESTIMATE SENTIMENT
# =============================================
# https://www.rdocumentation.org/packages/twitteR/versions/1.1.9

numTweetsExtract <- 1000

library(tidyverse)
library(dplyr)
library(ROAuth)
library(twitteR)
library(syuzhet)

print(paste("Extraction started at",Sys.time()))

# ============ Load sentiment parameters seed:
allFeatures_df <- readRDS("data/scopeExtraction.rds")
sentimentTerms <- allFeatures_df %>% filter(source %in% c("twitterSentiment"))
tmpItems <- as.list(sentimentTerms$termsDetailed)
names(tmpItems) <- sentimentTerms$variable
sentimentTerms <- tmpItems
sentimentTerms

# connect to twitter API from the twitter package
setup_twitter_oauth(twitter_api_key, twitter_api_secret, twitter_access_token, twitter_access_token_secret)


# =============================================
# TREAT TEXT AND CALCULATE SENTIMENT
# =============================================
# Clean text of tweets extracted ============
cleanTexts <- function(dfTexts) {
  dfTexts$text <- gsub('https://','',dfTexts$text) # removes https://
  dfTexts$text <- gsub('http://','',dfTexts$text) # removes http://
  dfTexts$text <- gsub('[^[:graph:]]', ' ',dfTexts$text) ## removes graphic characters like emoticons
  dfTexts$text <- str_replace_all(dfTexts$text,"[^[:graph:]]", " ")
  dfTexts$text <- gsub("[[:punct:]]", "", dfTexts$text) # removes punctuation (and @)
  dfTexts$text <- gsub("[[:cntrl:]]", "", dfTexts$text) # removes control characters
  dfTexts$text <- gsub("\\d+", "", dfTexts$text) # removes numbers
  dfTexts$text <- tolower(dfTexts$text)
  dfTexts$text <- gsub("… ", "", dfTexts$text) # clean "..."
  dfTexts <- distinct(dfTexts, screenName, .keep_all = TRUE) # remove repeated 
}
# Extract tweets texts ============
extractTermTweets <- function(term, KAM) {
  if (term=="") {   #!!!!!
    term <- KAM
  }
  tweetsList <- searchTwitter(term, resultType="mixed", n=numTweetsExtract)
  print(paste0("Retrieving tweets related with ",KAM,"::",term))
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
# Process list of terms within each KAM (Key Assets to Manage) ============
}
analyzeListTerms <- function(listTerms, listName) {
  lapply(listTerms, function(j) extractTermTweets(j, listName) %>% 
           bind_rows())
}

# =============================================
# CALL TEXTS EXTRACTION AND CALCULATE SENTIMENT
# =============================================
allTexts.df <- 
  lapply(seq_along(sentimentTerms), function(i) analyzeListTerms(sentimentTerms[[i]],names(sentimentTerms)[[i]])) %>% 
  bind_rows() %>% 
  group_by(date, term, KAM) %>% 
  summarise(score=mean(score))
head(allTexts.df)

# =============================================
# READ THE TIDY VERSION AS BASE FOR INCREMENTAL ADDITION
# =============================================
# In case features change, we re-start storing historical archive:
# saveRDS(as_tibble(allTexts.df),"data/data_twitterSentiment_tidy.rds")  # when terms change we re-create historic file
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
  spread(key=KAM, value=score, fill=NA) %>% 
  mutate(countryCode=NA)
head(newTotalData_ts)
saveRDS(as_tibble(newTotalData_ts),"data/data_twitterSentiment_ts.rds")

print(paste("Extraction finished succesfully at",Sys.time()))









