# https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/use-twitter-api-r/
# https://cran.r-project.org/web/packages/rtweet/vignettes/intro.html

# install.packages("rtweet")

# load twitter library - the rtweet library is recommended now over twitteR
library(rtweet)
# plotting and pipes - tidyverse!
library(ggplot2)
library(dplyr)
# text mining library
library(tidytext)

# create token named "twitter_token"
twitter_token <- create_token(
  app = twitter_api_appname,
  consumer_key = twitter_api_key,
  consumer_secret = twitter_api_secret,
  access_token = twitter_access_token,
  access_secret = twitter_access_token_secret)




## search for 500 tweets using the #rstats hashtag
rstats_tweets <- search_tweets(q = "#rstats", n = 500)
names(rstats_tweets)
rstats_tweets$screen_name
unique(as.Date(rstats_tweets$created_at))
rstats_tweets$text




## stream london tweets for a week (60 secs x 60 mins * 24 hours *  7 days)
stream_tweets(
  "realdonaldtrump,trump",
  timeout = 60 * 60 * 24 * 7,
  file_name = "tweetsabouttrump.json",
  parse = FALSE
)
## read in the data as a tidy tbl data frame
djt <- parse_stream("tweetsabouttrump.json")



## lookup users by screen_name or user_id
users <- c("KimKardashian", "justinbieber", "taylorswift13",
           "espn", "JoelEmbiid", "cstonehoops", "KUHoops",
           "upshotnyt", "fivethirtyeight", "hadleywickham",
           "cnn", "foxnews", "msnbc", "maddow", "seanhannity",
           "potus", "epa", "hillaryclinton", "realdonaldtrump",
           "natesilver538", "ezraklein", "annecoulter")
famous_tweeters <- lookup_users(users)
## preview users data
famous_tweeters
# extract most recent tweets data from the famous tweeters
tweets_data(famous_tweeters)
