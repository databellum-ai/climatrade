# PTE: problema con le paquete "sentiment"
# PTE: crear lista de sinónimos de cada término
# PTE: fallo de código en línea: "tm_map(tweets_collections, content_transformer(functio...."
# PTE: añadir PositiveWordsResearch al prinicipio del sentiment analysis? (https://positivewordsresearch.com/list-of-positive-words/)
# PTE: valorar problema de sólo 10 días hacia atrás (¿pagar? / ¿mantener predicciones corto plazo? / ¿usar otra fuente... news?) COMPROBAR NIVEL ACTUAL EN TWITTER ("Elevated?"): https://developer.twitter.com/en/docs/twitter-api/getting-started/about-twitter-api#Access

# https://www.rpubs.com/JaimeFC/415349


install.packages("ROAuth")
install.packages("twitteR")
install.packages("stringr")
install.packages("tm")
install.packages("wordcloud2")  # to draw tag clouds


library(tidyverse)
library(ROAuth)
library(twitteR)
library(stringr)
library(tm)
library(wordcloud2)
# library(sentiment)
library(data.table)
library(ggthemes)
library(ggplot2)
library(plotly)

searchTerms <- c("Daimler", "@POTUS", "telecinco")

# ================================
# STEP 1: Extract words and frequency in twits
# ================================

i <- 1
searchTerms[1]

# connect to twitter API from the twitter package
setup_twitter_oauth(twitter_api_key, twitter_api_secret, twitter_access_token, twitter_access_token_secret)
tweets_found <- searchTwitter(searchTerms[i], n = 1000, resultType = "recent", lang = "en", retryOnRateLimit = 100)

# we convert the tweets_found from list to a data frame
tweets_found.df <- twListToDF(tweets_found)
head(tweets_found.df)
# We remove repeated and clean "..."
tweets_found.nodups.df <- distinct(tweets_found.df, text, .keep_all = TRUE)
tweets_found.nodups.df$text <- gsub("… ", "", tweets_found.nodups.df$text)
# We change the feature created to Date
tweets_found.nodups.df <- plyr::rename(tweets_found.nodups.df, c(created = "Date"))
tweets_found.nodups.df$Date <- as.Date(tweets_found.nodups.df$Date)
# We transform from datetime to date format
tweets_text <- lapply(tweets_found, function(x) x$getText())
# We delete the (retweets)
tweets_unique <- unique(tweets_text)
# We remove the emoticons
tweets_text <- sapply(tweets_text, function(row) iconv(row, "latin1", "ASCII", sub = "byte"))
# We create the tweets collection with the found tweets to use into the sentiment analysis
functionalText = str_replace_all(tweets_text, "[^[:graph:]]", " ")
# Distinct the words from the sentence to use them on word cloud plot
tweets_collections <- Corpus(VectorSource(tweets_unique))
functionalText <- str_replace_all(tweets_collections, "[^[:graph:]]", " ")
# !! tweets_collections <- tm_map(tweets_collections, content_transformer(function(x) iconv(x, to = "latin1", "ASCII", sub = "")))
# We change all words from capital to lower case
tweets_collections <- tm_map(tweets_collections, content_transformer(tolower))
# We delete all punctuations
tweets_collections <- tm_map(tweets_collections, removePunctuation)
# From tm package we use the below functions to return various kinds of stopwords with support for different languages.
tweets_collections <- tm_map(tweets_collections, function(x) removeWords(x, stopwords()))
tweets_collections <- tm_map(tweets_collections, removeWords, stopwords("english"))
tweets_collections <- tm_map(tweets_collections, removeNumbers)
tweets_collections <- tm_map(tweets_collections, stripWhitespace)
# From tm package we use the below functions to construct or coerce to a term-document matrix or a document-term matrix
term_matrix <- TermDocumentMatrix(tweets_collections, control = list(wordLengths = c(1, Inf)))
# view the terms
term_matrix
# From tm package we find frequent terms in a document-term or term-document matrix.
(frequency.terms <- findMostFreqTerms(term_matrix, lowfrequency = 20))
# Transform it to matrix
term_matrix <- as.matrix(term_matrix)
term_matrix[1:10,1:20]
# We compute the frequency of the distinct words from the tweets and we sort it by frequency
distinctWordsfrequency <- sort(rowSums(term_matrix), decreasing = T)
# Transform it to df
distinctWordsDF <- data.frame(names(distinctWordsfrequency), distinctWordsfrequency)
colnames(distinctWordsDF) <- c("word", "frequency")
head(distinctWordsDF)

# DRAW a TAG CLOUD
wordcloud2(distinctWordsDF)




# RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR


searchTerms <- c("Daimler", "@POTUS", "telecinco")

# ================================
# STEP 1: Extract words from Twitter
# ================================

i <- 1
searchTerms[1]

# connect to twitter API from the twitter package
setup_twitter_oauth(twitter_api_key, twitter_api_secret, twitter_access_token, twitter_access_token_secret)
tweets_found <- searchTwitter(searchTerms[i], n = 1000, resultType = "recent", lang = "en", retryOnRateLimit = 100)

# we convert the tweets_found from list to a data frame
tweets_found.df <- twListToDF(tweets_found)
head(tweets_found.df)
# We remove repeated and clean "..."
tweets_found.nodups.df <- distinct(tweets_found.df, text, .keep_all = TRUE)
tweets_found.nodups.df$text <- gsub("… ", "", tweets_found.nodups.df$text)
# We change the feature created to Date
tweets_found.nodups.df <- plyr::rename(tweets_found.nodups.df, c(created = "Date"))
tweets_found.nodups.df$Date <- as.Date(tweets_found.nodups.df$Date)
# We transform from datetime to date format
tweets_text <- lapply(tweets_found, function(x) x$getText())
# We delete the (retweets)
tweets_unique <- unique(tweets_text)

# ================================
# STEP 2: Score Sentiment Function
# ================================
# We create the score sentiment function in order to run it afterwards in our found tweets. Transform the vector with the sentences to simple array of scores with plyr package Then we clean the sentences with gsub() function Then we convert the letters from capital to lower case The below function was inspired by (https://medium.com/@rohitnair_94843/analysis-of-twitter-data-using-r-part-3-sentiment-analysis-53d0e5359cb8).

# Obtain Positive and Negative words reference listings
# We use the famous lexicon of posive and negative words that was created from : Liu, Bing, Minqing Hu, and Junsheng Cheng. “Opinion observer: analyzing and comparing opinions on the web.” In Proceedings of the 14th international conference on World Wide Web (WWW-2005), pp. 342-351. ACM, May 10-14, 2005. Thanks to Liu and Hu we will add more than 6500 positive phrases, idioms and quotes
# https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html#lexicon


hu.liu.negative = scan("data/opinion-lexicon-English/negative-words.txt", what="character", comment.char=";")
hu.liu.positive = scan("data/opinion-lexicon-English/positive-words.txt", what="character", comment.char=";")
PositiveWordsResearch <- NULL
pos.words <- c(hu.liu.positive, PositiveWordsResearch)
# We add the extra words we noticed
pos.words <- c(pos.words, "thanx", "awesome", "fantastic", "super", "prima", 
               "toll", "cool", "geil", "profit", "profits", "earnings", "congrats", "prizes", 
               "prize", "thanks", "thnx", "Grt", "gr8", "plz", "trending", "recovering", 
               "brainstorm", "leader")
neg.words <- c(hu.liu.negative, "avoid", "lose", "loses", "scandal", "dieselgate", 
               "sucks", "awful", "disgusting", "negative", "wait", "waiting", "hold", "onhold", 
               "on hold", "cancel", "spam", "spams", "cancel", "wth", "Fight", "fighting", 
               "wtf", "arrest", "no", "not")



score.sentiment = function(sentence, pos.words, neg.words, .progress = "none") {
  scores = lapply(sentences, function(sentence, pos.words, neg.words) {

    sentence = gsub('https://','',sentence) # removes https://
    sentence = gsub('http://','',sentence) # removes http://
    sentence = gsub('[^[:graph:]]', ' ',sentence) ## removes graphic characters like emoticons
    sentence = str_replace_all(sentence,"[^[:graph:]]", " ")
    
    sentence = gsub("[[:punct:]]", "", sentence) # removes punctuation 
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
  }, pos.words, neg.words, .progress = .progress)
  scores.df = data.frame(score = scores, text = sentences)
  return(scores.df)
  
}


# We apply the above sentiment function on our tweets
currentTerm.scores <- score.sentiment(tweets_unique, pos.words, neg.words, .progress = "none")
currentTerm.scores
sum(currentTerm.scores$score)


# ================================
# STEP 3: Sentiment Classification using Distant Supervision
# ================================
# We apply the “Sentiment Classification using Distant Supervision” to create the Sentiments this method was first applied by:Huang, Bhayani and Go Based on Huang, Bhayani and Go Twitter “Sentiment Classification using Distant Supervision” research paper for Stanford university published on Dec 18,2017 (https://www-cs.stanford.edu/people/alecmgo/papers/TwitterDistantSupervision09.pdf) They presented the results of machine learning algorithms for classifying the sentiment of Twitter messages using distant supervision. Their training data consists of Twitter messages with emoticons, which are used as noisy labels. This type of training data is abundantly available and can be obtained through automated means. We show that machine learning algorithms (Naive Bayes, Maximum Entropy, and SVM) have accuracy above 80% when trained with emoticon data.

sentiments <- sentiment(tweets_unique)
table(sentiments$polarity)
sentiments$score <- 0
sentiments$score[sentiments$polarity == "positive"] <- 1
sentiments$score[sentiments$polarity == "negative"] <- -1

sentiments$date <- as.IDate(tweets_found.nodups.df$Date)
result <- aggregate(score ~ date, data = sentiments, mean)
# We create an interactive plot with the  scoring results mean per date

plotresults <- ggplot(result, aes(x = date, y = score)) + xlab("Date") + ylab("Sentiment Scoring Mean") + 
  ggtitle("Sentiment Scoring Mean by Date") + theme_solarized(light = FALSE) + 
  geom_path(color = "yellow", size = 1) + geom_point(color = "red", size = 3)

ggplotly(plotresults)



