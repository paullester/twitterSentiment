#################################################
#########Sentiment Analysis on Tweets############
################# Paul Le Ster ##################
#################################################
#The purpose of this code is to build a simple sentiment analysis classifier
#and how sentiment fluctuated by month
#I use  the tidytext package developed by Julia Silge and David Robinson,
#I'm borrow heavily frmo the code in this article (which served as inspiration) http://varianceexplained.org/r/trump-tweets/

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


source("sentiment_helper.R")


################################
#Important note: In order to show a strong difference, I am filtering DJT's tweets to show only those posted from his Android
#According to analysis done here http://varianceexplained.org/r/trump-tweets/, they are much more likely to be direct from DJT
#To see tweets directly from Hillary, I am filtering for those ending in "-H"
#################################

################################
#GET RAW TWEETS
#Tweets are from Trump twitter archive
#hillary tweets are from allmytweets.com
#Pulled by copy and pasting into csv <- TODO: do through web scraping next time
#################################


#
#Trump
#

#Try for all tweets
djt.raw <- read.csv("rawtweetsv2.csv", header = FALSE, as.is=TRUE)
djt.df <- data.frame(djt.raw)

#initial split
djt.df<-cSplit(djt.df, 'V1', sep="\xca", type.convert=FALSE)
keeps <- c("V1_1", "V1_2","V1_3")

device <- NULL
for(i in 1:nrow(djt.df)) {
  device <- c(source,extractSource(djt.df[i,"V1_3"]))
}
djt.df$device = device
djt.df = subset(djt.df, select = -c(V1_3,V1_4,V1_5,V1_6) )

#Hacky Date fix
djt.df$dateraw = substr(djt.df$V1_1,1,nchar(djt.df$V1_1)-12)
djt.df$dateraw = trimws(djt.df$dateraw)
djt.df$month = substr(djt.df$dateraw,1,3)
djt.df$day = substr(djt.df$dateraw,5,6)
djt.df$day = gsub(',','',djt.df$day)
djt.df$year = substr(djt.df$V1_1,nchar(djt.df$dateraw)-3,nchar(djt.df$dateraw))
djt.df$date <- as.Date(paste(djt.df$day,djt.df$month,djt.df$year, sep=""), "%d%b%Y")
djt.df = subset(djt.df, select = -c(V1_1,day,month,year,dateraw) )

#add source column
djt.df$source = "Trump"
names(djt.df)[names(djt.df)=="V1_2"] <- "tweet"
#subset to match dates july 20th to nov 9th
djt.df.trim = djt.df[date>="2016-07-19" & date<="2016-11-10"]
djt.df.trim = djt.df.trim[device=="Android"]
djt.df.trim = subset(djt.df.trim, select = -c(device) )

#
#Hillary
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

hrc.df$source = "Hillary"

#subset to match dates july 20th to nov 9th
hrc.df.trim  <- subset(hrc.df, date > "2016-07-19" & date < "2016-11-10")

hrc.df.trim$fromHill = str_detect(hrc.df.trim$tweet, "-H") 

hrc.df.trim2<-subset(hrc.df.trim, fromHill == TRUE)

#hrc.df.trim$fromHill = str_detect(hrc.df.trim$tweet, "-Hillary")
                                              
#combine tweets
tweets.df <- rbind(hrc.df.trim2, djt.df.trim)

#add in an id column

tweets.df$id<-seq.int(nrow(tweets.df))

###########################
##Clean text data
##########################

#remove tweets startign with " -> indication of a manual retweet, which Trump does once in a while
colnames(tweets.df)[1] <- "text"
reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
tweet.words <- tweets.df %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

#count most frequent words by candidate, take log ratio differentces
djt_hrc_comparison_ratio <- tweet.words %>%
  count(word, source) %>%
  filter(sum(n) >= 5) %>%
  spread(source, n, fill = 0) %>%
  ungroup() %>%
  mutate_each(funs((. + 1) / sum(. + 1)), -word) %>%
  mutate(logratio = log2(Trump / Hillary)) %>%
  arrange(desc(logratio))


djt_hrc_comparison_ratio %>%
  group_by(logratio > 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Trump / Hillary log ratio") +
  scale_fill_manual(name = "", labels = c("Trump", "Hillary"),
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

djt_hrc_comparison_ratio %>%
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
  labs(x = "", y = "Trump / Hillary log ratio") +
  scale_fill_manual(name = "", labels = c("Trump", "Hillary"),
                    values = c("red", "lightblue"))


