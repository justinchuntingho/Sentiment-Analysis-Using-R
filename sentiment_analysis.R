# Title: Sentiment Analysis using R
# Author: Justin Chun-ting Ho
# Last Updated: 22 Nov 2018

####################################################################################
## Analysing Amazon Review                                                        ##
####################################################################################
library(quanteda)
library(magrittr)
library(tidytext)
library(ggplot2)
library(lubridate)
library(dplyr)

#################### Basic Text Analysis ####################
# Loading the .csv file
amazon <- read.csv("amazon.csv", stringsAsFactors = FALSE) 

# Creating a corpus object
amazon.corpus <-  corpus(amazon, text_field = "text")

# Creating a document-feature matrix
amazon.dfm <- dfm(amazon.corpus, 
                  remove_punct = TRUE, 
                  remove_numbers = TRUE, 
                  remove_url = TRUE,
                  remove = c(stopwords("english"), "br"))

# Generating the meta-data matrix
amazon.docvar <- docvars(amazon.corpus)

topfeatures(amazon.dfm)

#################### Sentiment Analysis ####################
# First we have to load the lexicon
bing <- get_sentiments("bing") # loading Bing Liu's Opinion Lexicon 
bingpos <- bing$word[bing$sentiment == "positive"] # Extracting all the positive words
bingneg <- bing$word[bing$sentiment == "negative"] # Extracting all the negative words
bingdict <- dictionary(list(positive = bingpos, # Turn them into a 'dictionary' object
                            negative = bingneg))

# Creating a "document-sentiment matrix" with the dictionary
amazon.sentiment <- dfm(amazon.dfm, dictionary = bingdict) 
amazon.sentiment.df <- as.data.frame(amazon.sentiment)
amazon.sentiment.df["sentiment"] <- amazon.sentiment.df$positive - amazon.sentiment.df$negative

# Merging the result with the existing meta-data matrix
amazon.docvar <- cbind(amazon.docvar, amazon.sentiment.df)

# Visualising the result
ggplot(amazon.docvar, aes(Score, sentiment)) +
  geom_jitter(width = 0.1, alpha = 0.5, color = "brown1") +
  geom_smooth(method = "lm", color = "chartreuse4", alpha = 0.8)

# There is a correlation between the comment and the score
cor(amazon.docvar$Score, amazon.docvar$sentiment)

# Finding the most positive texts
amazon.docvar %>% arrange(desc(sentiment)) %>% head(5)
amazon[847,]

# Finding the most negative texts
amazon.docvar %>% arrange(sentiment) %>% head(5)
amazon[395,]

####################################################################################
## Analysing Facebook Posts                                                       ##
####################################################################################

#################### Basic Text Analysis and House Keeping ####################
# Reading .csv and creating corpus object
snp <- read.csv("SNP_corpus.csv", stringsAsFactors = FALSE)
snp.corpus <-  corpus(snp, text_field = "post_message")

# Calculating the number of words of each posts
docvars(snp.corpus, "ntoken") <- ntoken(snp.corpus)

# tranform the data to Date format
docvars(snp.corpus, "date") <- lubridate::as_date(docvars(snp.corpus, "post_published"))

# Creating document-feature matrix
snp.dfm <- dfm(snp.corpus, remove_punct = TRUE, remove_numbers = TRUE, verbose = TRUE, remove_url = TRUE, stem = FALSE, remove = c(stopwords("english"), "facebook", "s"))

# Generating the meta-data matrix
snp.docvar <- docvars(snp.corpus)

# Basic Exploration
topfeatures(snp.dfm)

# Basic Wordcloud
textplot_wordcloud(snp.dfm)

#################### Sentiment Analysis ####################

# Creating a "document-sentiment matrix" and merging the result with the meta-data matrix
snp.sentiment <- dfm(snp.dfm, dictionary = bingdict)
snp.sentiment.df <- as.data.frame(snp.sentiment)
snp.sentiment.df["sentiment"] <- snp.sentiment.df$positive - snp.sentiment.df$negative
snp.docvar <- cbind(snp.docvar, snp.sentiment.df)

# Checking the most positive texts
snp.docvar %>% arrange(desc(sentiment)) %>% head(5)
snp[193,]

# Checking the most negative texts
snp.docvar %>% arrange(sentiment) %>% head(5)
snp[146,]

# How many posts are positive? How many are negative?
ggplot(snp.docvar, aes(x = sentiment)) +
  geom_histogram(binwidth = 1)

# To visualising sentiment by time, first we have to do some data wranggling
sentiment_bydate <- snp.docvar %>% 
  select(date, ntoken, sentiment) %>% 
  group_by(date) %>% 
  summarize(ntoken = sum(ntoken), 
            sentiment = sum(sentiment)/sum(ntoken)) # normalise it (dividing by length of the post)

ggplot(sentiment_bydate, aes(date, sentiment, color = sentiment)) + 
  geom_point(alpha = 0.7) +
  geom_smooth(method = "loess", alpha = 0.5) +
  labs(title="Sentiment of SNP Posts", x ="Date", y = "Sentiment")

# What's more?
# There is another lexicon within the tidytext package, try:

get_sentiments("nrc")

# The NRC lexicon contain words for emotion, you could try and apply the lexicon by
# altering the above codes.
  



