remove(list=ls())

data <- read.csv("C:/Users/Shreyansh Mohanty/Desktop/IIM Ranchi MBA-BA Files/Term-6/TAUR/R Programs/Datasets/Snapchat_app_store_reviews.csv")


library(tidytext)
library(textdata)
library(tm)
library(dplyr)
library(purrr)

# Un-nesting tokens by words...

tokens_words <- data %>% unnest_tokens(word,review) %>% count(userName,word, sort = TRUE) %>% ungroup()

# Removing stop words...

clean_review <- data %>% 
  mutate(review_updated = tolower(review)) %>% 
  mutate(review_updated = gsub("[^[:alnum:]\\s]", "", review)) %>% 
  mutate(review_updated = gsub("\\s+", " ", review)) %>% 
  mutate(review_updated = gsub("\\b\\w{1,2}\\b", "", review)) %>% 
  mutate(review_updated = removeWords(review_updated, stopwords("english")))


data(stop_words)
tokens_words <- tokens_words %>% anti_join(stop_words)

# TF-IDF...
tokens_words_tfidf <- tokens_words %>% bind_tf_idf(word, userName, n)

# Removing custom stop-words (Snapchat, App)