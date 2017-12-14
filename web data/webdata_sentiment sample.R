# SPDS: Web data 
# Example: Twitter
# Theme: Sentiment Analysis

# Install packages to prepare for collecting and analyzing web data (Twitter)
install.packages("RColorBrewer")
install.packages("tm")
install.packages('base64enc')
install.packages('ROAuth')
install.packages('plyr')
install.packages('stringr')
install.packages('twitteR')

# Load libraries

library(RColorBrewer)
library(tm)
library(twitteR)
library(ROAuth)
library(plyr)
library(stringr)
library(base64enc)

# Twitter Authentication (after getting Twitter account and acquire credentials
#                        from https://dev.twitter.com/apps)
consumer_key <- "YourConsumerKey"
consumer_secret <- "YourConsumerSecret"
access_token <- "YourAccessToken"
access_secret <- "YourAccessSecret"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# Collect Tweets from Dallas

library(ggmap)
latlon <- geocode("dallas, tx")

# Be sure to reverse lat and lon
tweetChinatrade <- searchTwitter("#China trade",  n=5000, geocode='lon,lat, 100mi')
str(tweetChinatrade)
str(head(tweetChinatrade,1))
length(tweetChinatrade)

# Organize the Twitter raw data into DF
tweetCTDF <- do.call("rbind", lapply(tweetChinatrade, as.data.frame))

# Sentiment analysis
install.packages("lubridate")
library(lubridate)
install.packages("tidytext")
library(tidytext)
library(dplyr)
install.packages("tokenizers")
library(tokenizers)
library(ggplot2)
library(tidyr)

twttextDF <- data_frame(text=tweetCTDF$text)
tidytwt= twttextDF %>%
  unnest_tokens(word, text)

# Call in the stop word dictionary

data(stop_words)
tidytwt <- tidytwt %>%  anti_join(stop_words)
tidytwt %>%
  count(word,sort=T)

tidytwt %>%
  count(word, sort = TRUE) %>%
  filter(n > 200) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

tidytwt <- tidytwt %>%
  mutate(linenumber = row_number())

# Sort out positive and negative keywords
sentiment_CTT <- tidytwt %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# Plot sentments
ggplot(sentiment_CTT, aes(index, sentiment)) +
  geom_col(show.legend = FALSE)

# Plot separate positive and negative uses
bing_word_counts <- tidytwt %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
