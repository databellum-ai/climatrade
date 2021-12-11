# PTE: conflicto entre ply y dplyr
# PTE: lista asociada de sinónimos de cada término
# PTE: distinguir entre términos from: y about:
# PTE: añadir PositiveWordsResearch al prinicipio del sentiment analysis? (https://positivewordsresearch.com/list-of-positive-words/)
# PTE: fechas: valorar con Twitter problema de sólo 10 días hacia atrás (¿pagar? / ¿mantener predicciones corto plazo? / ¿usar otra fuente... news?) COMPROBAR NIVEL ACTUAL EN TWITTER ("Elevated?"): https://developer.twitter.com/en/docs/twitter-api/getting-started/about-twitter-api#Access


# https://www.rpubs.com/JaimeFC/415349
# https://rstudio-pubs-static.s3.amazonaws.com/276096_5f10707713d7468fb5a3834a85c2de72.html

# https://cran.r-project.org/web/packages/rtweet/vignettes/intro.html

searchTerms <- c("Daimler", "@POTUS", "Amazon")

library(tidyverse)
library(plyr)
library(dplyr)
library(ROAuth)
library(twitteR)
library(stringr)


# connect to twitter API from the twitter package
setup_twitter_oauth(twitter_api_key, twitter_api_secret, twitter_access_token, twitter_access_token_secret)


# ================================
# STEP 1: Obtain Positive and Negative words reference listings
# ================================  
# We use the famous lexicon of posive and negative words that was created from : Liu, Bing, Minqing Hu, and Junsheng Cheng. “Opinion observer: analyzing and comparing opinions on the web.” In Proceedings of the 14th international conference on World Wide Web (WWW-2005), pp. 342-351. ACM, May 10-14, 2005. Thanks to Liu and Hu we will add more than 6500 positive phrases, idioms and quotes
# https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html#lexicon
hu.liu.negative = scan("data/opinion-lexicon-English/negative-words.txt", what="character", comment.char=";")
hu.liu.positive = scan("data/opinion-lexicon-English/positive-words.txt", what="character", comment.char=";")
PositiveWordsResearch <- NULL
# We add the extra words we noticed
pos.words <- c(hu.liu.positive, PositiveWordsResearch)
pos.words <- c(pos.words, "thanx", "awesome", "fantastic", "super", "prima", 
               "toll", "cool", "geil", "profit", "profits", "earnings", "congrats", "prizes", 
               "prize", "thanks", "thnx", "Grt", "gr8", "plz", "trending", "recovering", 
               "brainstorm", "leader")
neg.words <- c(hu.liu.negative, "avoid", "lose", "loses", "scandal", "dieselgate", 
               "sucks", "awful", "disgusting", "negative", "wait", "waiting", "hold", "onhold", 
               "on hold", "cancel", "spam", "spams", "cancel", "wth", "Fight", "fighting", 
               "wtf", "arrest", "no", "not")


# ================================
# STEP 2: Sentiment function
# ================================
# We create the score sentiment function in order to run it afterwards in our found tweets

score.sentiment = function(sentences, pos.words, neg.words, .progress = 'none') {
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    sentence = gsub('https://','',sentence) # removes https://
    sentence = gsub('http://','',sentence) # removes http://
    sentence = gsub('[^[:graph:]]', ' ',sentence) ## removes graphic characters like emoticons
    sentence = str_replace_all(sentence,"[^[:graph:]]", " ")
    
    sentence = gsub("[[:punct:]]", "", sentence) # removes punctuation (and @)
    sentence = gsub("[[:cntrl:]]", "", sentence) # removes control characters
    sentence = gsub("\\d+", "", sentence) # removes numbers
    sentence = tolower(sentence)
    # With stringr package we distinct it into words
    word.list = str_split(sentence, "\\s+")
    # We unlist the vector
    words = unlist(word.list)
    # We compare the words from found tweets, with the above positive and
    # negative words of the dictionaries
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    # The match function returned the position of the matched word otherwise NA
    # Remove NAs
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    # We tranform the matches into 1 or 0 with the sum fiunction
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df = data.frame(score = scores, text = sentences)
  return(scores.df)
}











numTweetsExtract <- 50
sentimentTerms <- list("miscelaneous" = c("from:potus", "Amazon", "from:sanchezcastejon"), 
                       "football" = c("real madrid", "messi"))

# FUNCTIONS ============
scoreTextSentiment <- function(t) {
  substr(t,1,15)
}
extractTermTweets <- function(term, KAM) {
  tweetsList <- searchTwitter(term, resultType="recent", n=numTweetsExtract)
  print(paste0(KAM,"::",term))
  twListToDF(tweetsList) %>% mutate(score=scoreTextSentiment(text), date = as.Date(created), term = term, KAM=KAM) %>% 
    select(date, score, term, KAM)
}
analyzeListTerms <- function(listTerms, listName) {
  lapply(listTerms, function(j) extractTermTweets(j, listName) %>% 
           bind_rows())
}

allTexts <- 
  lapply(seq_along(sentimentTerms), function(i) analyzeListTerms(sentimentTerms[[i]],names(sentimentTerms)[[i]])) %>% 
  bind_rows()

head(allTexts)
view(allTexts)








# ================================
# STEP 3: Extract texts from Twitter
# ================================

# >>>>>
# >>>>>
scores.df <- data.frame()

for (i in c(1:length(searchTerms))) {
  print(paste("Reading tweets:",searchTerms[i]))

  tweets_found <- 
    searchTwitter(searchTerms[i], n = 50, resultType = "popular", lang = "en", retryOnRateLimit = 100)
  # convert the tweets_found from list to a data frame
  tweets_found.df <- twListToDF(tweets_found)
  print(paste("Tweets found:",nrow(tweets_found.df)))
  head(tweets_found.df)
  tweets_found.df$text
  tweets_found.nodups.df$text
  
  # remove repeated and clean "..."
  tweets_found.nodups.df <- distinct(tweets_found.df, text, .keep_all = TRUE)
  tweets_found.nodups.df$text <- gsub("… ", "", tweets_found.nodups.df$text)
  # change the feature created to Date
  tweets_found.nodups.df <- tweets_found.nodups.df %>% 
    mutate(created = as.Date(tweets_found.nodups.df$created)) 
  names(tweets_found.nodups.df)[names(tweets_found.nodups.df) == "created"] <- "date"
  # transform from datetime to date format
  tweets_text <- lapply(tweets_found, function(x) x$getText())
  # delete the (retweets)
  tweets_unique <- unique(tweets_text)
  # obtain cleaned tweets and their dates
  length(tweets_found.nodups.df$date)
  length(tweets_unique)
  
  # We apply the above sentiment function on our tweets
  currentTerm.scores <- score.sentiment(tweets_unique, pos.words, neg.words, .progress = "none")
  class(currentTerm.scores)
  
  # We obtain sentiment polarity for twits including date
  tmpScores <- 
    data.frame(date = tweets_found.nodups.df$date, term = searchTerms[i], score = currentTerm.scores$score)
  tmpScores
  # accumulate results
  scores.df <- rbind(scores.df, tmpScores)
  scores.df
  tmpScores <- NULL
}
# <<<<<
# <<<<<

# if("dplyr" %in% (.packages())){
#   detach("package:dplyr", unload=TRUE) 
#   detach("package:plyr", unload=TRUE) 
# } 
# library(plyr)
# library(dplyr)

scores.df <- scores.df %>% 
  group_by(date, term) %>% 
  summarise(score=mean(score))
head(scores.df)

scores.df_ts <- scores.df %>% 
  spread(key = term, value = score, fill=NA)
head(scores.df_ts)
