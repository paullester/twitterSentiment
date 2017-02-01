#################################################
#########Sentiment Analysis on Tweets############
################# Paul Le Ster ##################
#################################################
#The purpose of this code is to build a simple sentiment analysis classifier
#and how sentiment fluctuated by month

library(dplyr)
library(purrr)
library(twitteR)
library(ggplot2)
library(stringr)
library(tidytext)
library(tidyr)
library(lubridate)
library(scales)
library(ggthemes)
library(extrafont)
library(splitstackshape)
#library(plyr)
library(dplyr)




################################
#GET RAW TWEETS
#Tweets are from Trump twitter archive
#hillary tweets are from allmytweets.com
#Pulled by copy and pasting into csv <- TODO: do through web scraping next time
#################################

#Try for all tweets
tweets.raw <- read.csv("rawtweetsv2.csv", header = FALSE, as.is=TRUE)
tweets.df <- data.frame(tweets.raw)

#initial split
tweets.df<-cSplit(tweets.df, 'V1', sep="\xca", type.convert=FALSE)
keeps <- c("V1_1", "V1_2","V1_3")
tweets.df = subset(tweets.df, select = -c(V1_4,V1_5,V1_6) )


#Hacky Date fix
tweets.df$dateraw = substr(tweets.df$V1_1,1,nchar(tweets.df$V1_1)-12)
tweets.df$dateraw = trimws(tweets.df$dateraw)
tweets.df$month = substr(tweets.df$dateraw,1,3)
tweets.df$day = substr(tweets.df$dateraw,5,6)
tweets.df$day = gsub(',','',tweets.df$day)
tweets.df$year = substr(tweets.df$V1_1,nchar(tweets.df$dateraw)-3,nchar(tweets.df$dateraw))
tweets.df$date <- as.Date(paste(tweets.df$day,tweets.df$month,tweets.df$year, sep=""), "%d%b%Y")
tweets.df = subset(tweets.df, select = -c(V1_1,day,month,year,dateraw) )


extractSource <- function(source.string) {
  source.string <- as.character(source.string)
  if (is.na(source.string)){
    return ("None")
  } else if (str_detect(source.string, "iPhone")){
    return("iPhone")
  } else if (str_detect(source.string, "Web")){
    return("Web")
  } else if (str_detect(source.string, "Android")){
    return("Android")
  } else {
    return ("None")
  }
}


source <- NULL
for(i in 1:nrow(tweets.df)) {
  source <- c(source,extractSource(tweets.df[i,"V1_3"]))
}
tweets.df$source = source


#add in an id column

tweets.df$id<-seq.int(nrow(tweets.df))




#
#Hillary
#
#


#Try for all tweets
hrc.raw <- read.csv("hillary_tweets_jul20onwards_raw.csv", header = FALSE, as.is=TRUE)
hrc.df <- data.frame(hrc.raw)

hrc.df$tweet = substr(hrc.df$V1,1,nchar(hrc.df$V1)-12)


#Hacky Date fix
hrc.df$dateraw = substr(hrc.df$V1,nchar(hrc.df$V1)-12,nchar(hrc.df$V1))
hrc.df$dateraw = trimws(hrc.df$dateraw)
hrc.df$month = substr(hrc.df$dateraw,1,3)
hrc.df$day = substr(hrc.df$dateraw,5,6)
hrc.df$year = substr(hrc.df$dateraw,nchar(hrc.df$dateraw)-3,nchar(hrc.df$dateraw))
hrc.df$date <- as.Date(paste(hrc.df$day,hrc.df$month,hrc.df$year, sep=""), "%d%b%Y")
hrc.df = subset(hrc.df, select = -c(V1,day,month,year,dateraw) )


extractSource <- function(source.string) {
  source.string <- as.character(source.string)
  if (is.na(source.string)){
    return ("None")
  } else if (str_detect(source.string, "iPhone")){
    return("iPhone")
  } else if (str_detect(source.string, "Web")){
    return("Web")
  } else if (str_detect(source.string, "Android")){
    return("Android")
  } else {
    return ("None")
  }
}


source <- NULL
for(i in 1:nrow(tweets.df)) {
  source <- c(source,extractSource(tweets.df[i,"V1_3"]))
}
tweets.df$source = source




###########################
##Clean text data
##########################

#remove tweets startign with " -> indication of a manual retweet
colnames(tweets.df)[1] <- "text"
reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
tweet.words <- tweets.df %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))


android_iphone_ratios <- tweet.words %>%
  count(word, source) %>%
  filter(sum(n) >= 5) %>%
  spread(source, n, fill = 0) %>%
  ungroup() %>%
  mutate_each(funs((. + 1) / sum(. + 1)), -word) %>%
  mutate(logratio = log2(Android / iPhone)) %>%
  arrange(desc(logratio))

android_iphone_ratios %>%
  group_by(logratio > 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Android / iPhone log ratio") +
  scale_fill_manual(name = "", labels = c("Android", "iPhone"),
                    values = c("red", "lightblue"))




nrc <- sentiments %>%
  filter(lexicon == "nrc") %>%
  dplyr::select(word, sentiment)

nrc

sources <- tweet.words %>%
  group_by(source) %>%
  mutate(total_words = n()) %>%
  ungroup() %>%
  distinct(id, source, total_words)

by_source_sentiment <- tweet.words %>%
  inner_join(nrc, by = "word") %>%
  count(sentiment, id) %>%
  ungroup() %>%
  complete(sentiment, id, fill = list(n = 0)) %>%
  inner_join(sources) %>%
  group_by(source, sentiment, total_words) %>%
  summarize(words = sum(n)) %>%
  ungroup()

head(by_source_sentiment)


library(broom)

sentiment_differences <- by_source_sentiment %>%
  group_by(sentiment) %>%
  do(tidy(poisson.test(.$words, .$total_words)))

sentiment_differences


android_iphone_ratios %>%
  inner_join(nrc, by = "word") %>%
  filter(!sentiment %in% c("positive", "negative")) %>%
  mutate(sentiment = reorder(sentiment, -logratio),
         word = reorder(word, -logratio)) %>%
  group_by(sentiment) %>%
  top_n(10, abs(logratio)) %>%
  ungroup() %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  facet_wrap(~ sentiment, scales = "free", nrow = 2) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "", y = "Android / iPhone log ratio") +
  scale_fill_manual(name = "", labels = c("Android", "iPhone"),
                    values = c("red", "lightblue"))


