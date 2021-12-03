# Load Twitter data

# https://www.rdocumentation.org/packages/twitteR/versions/1.1.9

# https://analytics4all.org/2016/11/16/r-connect-to-twitter-with-r/


# ~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~
install.packages("twitteR")
library(twitteR)


setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

# ~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~

# EXAMPLE 1:

searchTwitter("#islatentaciones")

resultTwits <- searchTwitter(searchString = "@potus", 
              n = 100, 
              lang = "en", 
              since = NULL, 
              until = NULL, 
              geocode = "37.781157,-122.39720,10mi", 
              sinceID	= NULL, maxID = NULL, resultType = "recent", retryOnRateLimit=10)


resultTwits
class(resultTwits)
length(resultTwits)
class(resultTwits[[1]])
length(resultTwits[[1]])
resultTwits[[1]]

# ~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~
# EXAMPLE 2:
# https://rpubs.com/Papacosmas/sentiments


library(utils)
 library(GGally)
library(devtools)
library(twitteR)
 library(tm)
 library(wordcloud2)
library(plyr)
library(stringr)
library(data.table)
library(dplyr)
library(ggthemes)
library(plotly)
library(plotrix)
library(httr)
 library(ROAuth)
library(dplyr)
 library(NLP)
 library(wordcloud2)
library(base64enc)
 library(wordcloud)
library(plotrix)
library(ggplot2)
library(lattice)
library(devtools)
library(rjson)
library(bit64)


require(ROAuth)
require(twitteR)


# from the twitter package we use the searchTwitter function to extract
# tweets ..  Tweeter free api allows only the tweets of the last 10 days
# resultType you can also change it to popular. With AND, OR you can add
# more search keywords

tweets_daimler = searchTwitter("Daimler", n = 10000, resultType = "mixed", lang = "en", 
                               retryOnRateLimit = 100)

# we convert the tweets_daimler from list to a data frame
(n.tweet <- length(tweets_daimler))
tweets_daimler.df <- twListToDF(tweets_daimler)
head(tweets_daimler.df)

require(dplyr)
tweets_daimler.nodups.df <- distinct(tweets_daimler.df, text, .keep_all = TRUE)
tweets_daimler.nodups.df$text <- gsub("… ", "", tweets_daimler.nodups.df$text)
# We change the feature created to Date
tweets_daimler.nodups.df <- plyr::rename(tweets_daimler.nodups.df, c(created = "Date"))
tweets_daimler.nodups.df$Date <- as.Date(tweets_daimler.nodups.df$Date)
# We transform from datetime to date format
tweets_text <- lapply(tweets_daimler, function(x) x$getText())
# We delete the (retweets)
tweets_unique <- unique(tweets_text)

# We remove the emoticons
tweets_text <- sapply(tweets_text, function(row) iconv(row, "latin1", "ASCII", 
                                                       sub = "byte"))

require(stringr)
functionalText = str_replace_all(tweets_text, "[^[:graph:]]", " ")
# We create the tweets collection with the daimler tweets to use into the
# sentiment analysis
require(tm)

tweets_collections <- Corpus(VectorSource(tweets_unique))
# Distinct the words from the sentence to use them on word cloud plot Ignore
# the warnings into the following scripts

functionalText = str_replace_all(tweets_collections, "[^[:graph:]]", " ")

tweets_collections <- tm_map(tweets_collections, content_transformer(function(x) iconv(x, 
                                                                                       to = "latin1", "ASCII", sub = "")))

# We change all words from capital to lower case
tweets_collections <- tm_map(tweets_collections, content_transformer(tolower))

# We delete all punctuations
tweets_collections <- tm_map(tweets_collections, removePunctuation)

# From tm package we use the below functions to return various kinds of
# stopwords with support for different languages.
tweets_collections <- tm_map(tweets_collections, function(x) removeWords(x, 
                                                                         stopwords()))
tweets_collections <- tm_map(tweets_collections, removeWords, stopwords("english"))
tweets_collections <- tm_map(tweets_collections, removeNumbers)
tweets_collections <- tm_map(tweets_collections, stripWhitespace)

# From tm package we use the below functions to construct or coerce to a
# term-document matrix or a document-term matrix
term_matrix <- TermDocumentMatrix(tweets_collections, control = list(wordLengths = c(1, 
                                                                                     Inf)))

# view the terms
term_matrix

# From tm package we find frequencyuent terms in a document-term or
# term-document matrix.
(frequency.terms <- findfrequencyTerms(term_matrix, lowfrequency = 20))
# Transform it to matrix
term_matrix <- as.matrix(term_matrix)

# We compute the frequency of the distinct words from the tweets and we sort
# it by frequency
distinctWordsfrequency <- sort(rowSums(term_matrix), decreasing = T)

# Transform it to df
distinctWordsDF <- data.frame(names(distinctWordsfrequency), distinctWordsfrequency)
colnames(distinctWordsDF) <- c("word", "frequency")