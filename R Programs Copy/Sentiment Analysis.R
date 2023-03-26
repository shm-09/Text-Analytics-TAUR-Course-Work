remove(list=ls())

library(tidytext)
library(textdata)
sentiments
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

library(janeaustenr)
library(dplyr)
library(stringr)
tidy_books <- austen_books() %>% group_by(book) %>% mutate(linenumber = row_number(),
                                                           chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>%
  ungroup() %>% unnest_tokens(word, text)

##################Happy####################

nrcjoy <- get_sentiments("nrc") %>% filter(sentiment == "joy")
tidy_books %>%
  filter(book == "Emma") %>% inner_join(nrcjoy) %>% count(word, sort = TRUE)

##################Sad####################

################Anger####################

################Fear#####################

################Trust####################

##############Sentiments from several pages taken together #####################

library(tidyr)
janeaustensentiment <- tidy_books %>% inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>% spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

library(ggplot2)
ggplot(janeaustensentiment, aes(index, sentiment, fill = book)) + geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")

###################Comparing three sentiment dictionaries#######################

pride_prejudice <- tidy_books %>% filter(book == "Pride & Prejudice")
pride_prejudice

afinn <- pride_prejudice %>% inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 80) %>% summarise(sentiment = sum(value)) %>% mutate(method = "AFINN")

bing_and_nrc <- bind_rows( 
  pride_prejudice %>% 
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."), 
  pride_prejudice %>% 
    inner_join(get_sentiments("nrc") %>% 
                 filter(sentiment %in%  c("positive", 
                                          "negative"))) %>%
    mutate(method = "NRC")) %>%
    count(method, index = linenumber %/% 80, sentiment) %>% 
    spread(sentiment, n, fill = 0) %>% 
    mutate(sentiment = positive - negative)

bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) + geom_col(show.legend = FALSE) + 
  facet_wrap(~method, ncol = 1, scales = "free_y")

################which dictionary has more positive and negative words###########

get_sentiments("nrc") %>% filter(sentiment %in% c("positive","negative")) %>%count(sentiment)
get_sentiments("bing") %>% count(sentiment)

#######################Most Common Positive and Negative Words##################

bing_word_counts <- tidy_books %>% inner_join(get_sentiments("bing")) %>% count(word, sentiment, sort = TRUE) %>% ungroup()
bing_word_counts

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>% ggplot(aes(word, n, fill = sentiment)) + 
  geom_col(show.legend = FALSE) + facet_wrap(~sentiment, scales = "free_y") + 
  labs(y = "Contribution to sentiment",x=NULL)+ coord_flip()


custom_stop_words <- bind_rows(data_frame(word = c("miss"), lexicon = c("custom")),stop_words)
custom_stop_words

##################################Word cloud####################################

library(wordcloud)
tidy_books %>%
  anti_join(custom_stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

library(reshape2)
tidy_books %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% comparison.cloud(colors = c("gray20", "gray80"),
                                                                          max.words = 100)


########################Looking at tokens beyond words##########################

PandP_sentences <- data_frame(text = prideprejudice) %>% unnest_tokens(sentence, text, 
                                                                       token = "sentences")

PandP_sentences$sentence[2]

#########We can also tokenise by regex pattern: Chapter numbers#################

austen_chapters <- austen_books() %>% group_by(book) %>%
  unnest_tokens(chapter, text, token = "regex",
                pattern = "Chapter|CHAPTER [\\dIVXLC]") %>% ungroup()
austen_chapters %>% group_by(book) %>% summarise(chapters = n())

bingnegative <- get_sentiments("bing") %>% filter(sentiment == "negative")
wordcounts <- tidy_books %>% group_by(book, chapter) %>% summarize(words = n())
temp <- tidy_books %>%
  semi_join(bingnegative) %>%
  group_by(book, chapter) %>%
  summarize(negativewords = n()) %>% left_join(wordcounts, by = c("book", "chapter")) %>% mutate(ratio = negativewords/words) %>% filter(chapter != 0) %>%
  top_n(2) %>% ungroup()


#############################Twitter sentiment analysis##########################

twitter <- read.csv("/Users/poojasengupta/Documents/Teaching/Text Analytics/Twitter customer support.csv", header = TRUE)
str(twitter)

nrcjoy <- get_sentiments("nrc") %>% filter(sentiment == "joy")
nrc_twitter %>% inner_join(nrcjoy) %>% count(word, sort = TRUE)
nrcanger <- get_sentiments("nrc") %>% filter(sentiment == "anger")
nrc_twitter %>% inner_join(nrcanger) %>% count(word, sort = TRUE)

afinn_twitter <- twitter %>%
  unnest_tokens(word, text, token = "tweets") %>%
  inner_join(get_sentiments("afinn"))

bing_twitter <- twitter %>%
  unnest_tokens(word, text, token = "tweets") %>%
  inner_join(get_sentiments("bing"))

nrc_twitter <- twitter %>%
  unnest_tokens(word, text, token = "tweets") %>%
  inner_join(get_sentiments("nrc"))

bing_word_counts <- bing_twitter %>% inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% ungroup()
bing_word_counts

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>% ggplot(aes(word, n, fill = sentiment)) + 
  geom_col(show.legend = FALSE) + facet_wrap(~sentiment, scales = "free_y") + 
  labs(y = "Contribution to sentiment",x=NULL)+ coord_flip()
       

custom_stop_words <- bind_rows(data_frame(word = c("issues","shit","fucking"), lexicon = c("custom")),stop_words)
custom_stop_words                                                                                                                                                         

library(wordcloud)
bing_twitter %>%
  anti_join(custom_stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

library(reshape2)
bing_twitter %>%
  anti_join(custom_stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% comparison.cloud(colors = c("gray20", "gray80"),
                                                                          max.words = 100)


###########################Twitter and Reddit#########################
remove(list=ls())

library(tidytext)
library(textdata)
sentiments
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

#library(janeaustenr)
library(dplyr)
library(stringr)

twitter_data <- read.csv("C:/Users/Shreyansh Mohanty/Desktop/IIM Ranchi MBA-BA Files/Term-6/TAUR/R Programs/Datasets/Twitter_Data.csv", header = TRUE)
reddit_data <- read.csv("C:/Users/Shreyansh Mohanty/Desktop/IIM Ranchi MBA-BA Files/Term-6/TAUR/R Programs/Datasets/Reddit_Data.csv", header = TRUE)
str(twitter_data)
str(reddit_data)

library(tidyr)
sentiment_twitter <- twitter_data %>%
  unnest_tokens(word, clean_text) %>%
  inner_join(get_sentiments("bing")) 


sentiment_reddit <- reddit_data %>%
  unnest_tokens(word, clean_comment) %>%
  inner_join(get_sentiments("bing"))

bing_word_counts_twitter <- sentiment_twitter %>% inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% ungroup()
bing_word_counts_twitter

bing_word_counts_twitter %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>% ggplot(aes(word, n, fill = sentiment)) + 
  geom_col(show.legend = FALSE) + facet_wrap(~sentiment, scales = "free_y") + 
  labs(y = "Contribution to sentiment",x=NULL)+ coord_flip()

bing_word_counts_reddit <- sentiment_reddit %>% inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% ungroup()
bing_word_counts_reddit

bing_word_counts_reddit %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>% ggplot(aes(word, n, fill = sentiment)) + 
  geom_col(show.legend = FALSE) + facet_wrap(~sentiment, scales = "free_y") + 
  labs(y = "Contribution to sentiment",x=NULL)+ coord_flip()

library(wordcloud)
sentiment_twitter %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

library(reshape2)
sentiment_twitter %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% comparison.cloud(colors = c("gray20", "gray80"),
                                                                          max.words = 100)


library(wordcloud)
sentiment_reddit %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

library(reshape2)
sentiment_reddit %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% comparison.cloud(colors = c("gray20", "gray80"),
                                                                          max.words = 100)


