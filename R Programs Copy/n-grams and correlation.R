remove(list=ls())

library(dplyr)
library(tidytext)
library(janeaustenr)

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) 
austen_bigrams

tail(austen_bigrams)

########Most common bigrams##########
austen_bigrams %>% count(bigram, sort = TRUE)

##########removing stop words from bigrams#################

library(tidyr)
bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)
# new bigram counts:
bigram_counts <- bigrams_filtered %>% count(word1, word2, sort = TRUE)
bigram_counts
tail(bigram_counts)

bigrams_united <- bigrams_filtered %>% unite(bigram, word1, word2, sep = " ")
bigrams_united

#####################trigrams######################

trigram_temp <- austen_books() %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>% 
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word,!word2 %in% stop_words$word,!word3 %in% 
           stop_words$word) %>% count(word1, word2, word3, sort = TRUE)


##############Analyzing bigrams####################

bigrams_filtered %>% filter(word1 == "miss") %>% count(book, word2, sort = TRUE)

bigrams_filtered %>% filter(word2 == "street") %>% count(book, word1, sort = TRUE)
bigrams_filtered %>% filter(word2 == "street" | word2 == "str" | word2 == "st") %>% count(book, word1, sort = TRUE)

########################tf-idf#####################

bigram_tf_idf <- bigrams_united %>% count(book, bigram) %>% bind_tf_idf(bigram, book, n) %>% arrange(desc(tf_idf))
bigram_tf_idf

library(ggplot2)
bigram_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>% group_by(book) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(bigram, tf_idf, fill = book)) + geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") + coord_flip()

#########Using Bigrams to Provide Context in Sentiment Analysis#########

bigrams_separated %>% filter(word1 == "not") %>% count(word1, word2, sort = TRUE)

AFINN <- get_sentiments("afinn")
AFINN

not_words <- bigrams_separated %>% 
  filter(word1 == "not") %>% 
  inner_join(AFINN, by = c(word2 = "word")) %>% 
  count(word2, value, sort = TRUE) %>% 
  ungroup()
not_words

not_words %>% 
  mutate(contribution = n * value) %>% 
  arrange(desc(abs(contribution))) %>% 
  head(20) %>% 
  mutate(word2 = reorder(word2, contribution)) %>% 
  ggplot(aes(word2, n * value, fill = n * value > 0)) + 
  geom_col(show.legend = FALSE) +xlab("Words preceded by \"not\"") +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment score * number of occurrences") + 
  coord_flip()

negation_words <- c("not", "no", "never", "without")
negated_words <- bigrams_separated %>% filter(word1 %in% negation_words) %>% 
  inner_join(AFINN, by = c(word2 = "word")) %>% count(word1, word2, value, sort = TRUE) %>% 
  ungroup()

#################visualising n-grams################

library(igraph) # original counts
bigram_counts

# filter for only relatively common combinations
bigram_graph <- bigram_counts %>% filter(n > 20) %>% graph_from_data_frame()
bigram_graph

library(ggraph) 
set.seed(2021)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


set.seed(2020)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") + geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,arrow = a, end_cap = circle(.07, 'inches')) + geom_node_point(color = "lightblue", size = 5) +geom_node_text(aes(label = name), vjust = 1, hjust = 1) + theme_void()

##################counting and correlating#############################

austen_section_words <- austen_books() %>%
  filter(book == "Pride & Prejudice") %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)
austen_section_words

library(widyr)
# count words co-occuring within sections
word_pairs <- austen_section_words %>%
  pairwise_count(word, section, sort = TRUE)
word_pairs

word_pairs %>%
  filter(item1 == "elizabeth")

# we need to filter for at least relatively common words first
word_cors <- austen_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)
word_cors

word_cors %>%
  filter(item1 == "pounds")

word_cors %>%
  filter(item1 %in% c("elizabeth", "pounds", "married", "pride")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

set.seed(2016)
word_cors %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

#####################Musical instrument reviews#######################
remove(list=ls())

music <- read.csv("C:/Users/IMI/Desktop/Text Analytics/Musical_instruments_reviews.csv", header = TRUE)
colnames(music)
str(music)
music %>% unnest_tokens(output = word, input = reviewText)

music %>% 
  unnest_tokens(output = word, input = reviewText) %>% 
  count(word, sort = TRUE)
music %>% 
  unnest_tokens(output = word, input = reviewText) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) 

music %>% 
  unnest_tokens(output = word, input = reviewText) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>% 
  slice(1:10) %>% 
  ggplot() + geom_bar(aes(word, n), stat = "identity", fill = "#de5833") +
  theme_minimal() +
  labs(title = "Top unigrams of Musical instrument Reviews",
       subtitle = "using Tidytext in R",
       caption = "Data Source: Amazon")


music_bigrams <-  music %>%
  unnest_tokens(bigram, reviewText, token = "ngrams", n = 2) 
music_bigrams

########Most common bigrams##########
music_bigrams %>% count(bigram, sort = TRUE)

##########removing stop words from bigrams#################

library(tidyr)
bigrams_separated <- music_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)
# new bigram counts:
bigram_counts <- bigrams_filtered %>% count(word1, word2, sort = TRUE)
bigram_counts

bigrams_united <- bigrams_filtered %>% unite(bigram, word1, word2, sep = " ")
bigrams_united

#####################trigrams######################

music %>%
  unnest_tokens(trigram, reviewText, token = "ngrams", n = 3) %>% 
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word,!word2 %in% stop_words$word,!word3 %in% 
           stop_words$word) %>% count(word1, word2, word3, sort = TRUE)


##############Analyzing bigrams####################

bigrams_filtered %>% filter(word2 == "guitar") %>% count(reviewerName, word1, sort = TRUE)

bigram_tf_idf <- bigrams_united %>% count(reviewerName, bigram) %>% bind_tf_idf(bigram, reviewerName, n) %>% arrange(desc(tf_idf))
bigram_tf_idf

#########Using Bigrams to Provide Context in Sentiment Analysis#########

bigrams_separated %>% filter(word1 == "good") %>% count(word1, word2, sort = TRUE)

AFINN <- get_sentiments("afinn")
AFINN

good_words <- bigrams_separated %>% 
  filter(word1 == "good") %>% 
  inner_join(AFINN, by = c(word2 = "word")) %>% 
  count(word2, value, sort = TRUE) %>% 
  ungroup()
good_words

good_words %>% 
  mutate(contribution = n * value) %>% 
  arrange(desc(abs(contribution))) %>% 
  head(20) %>% 
  mutate(word2 = reorder(word2, contribution)) %>% 
  ggplot(aes(word2, n * value, fill = n * value > 0)) + 
  geom_col(show.legend = FALSE) +xlab("Words preceded by \"good\"") +
  xlab("Words preceded by \"good\"") +
  ylab("Sentiment score * number of occurrences") + 
  coord_flip()

positive_words <- c("good", "solid", "quality")
positive_words <- bigrams_separated %>% filter(word1 %in% positive_words) %>% 
  inner_join(AFINN, by = c(word2 = "word")) %>% count(word1, word2, value, sort = TRUE) %>% 
  ungroup()

#################visualising n-grams################

library(igraph) # original counts
bigram_counts

# filter for only relatively common combinations
bigram_graph <- bigram_counts %>% filter(n > 20) %>% graph_from_data_frame()
bigram_graph

library(ggraph) 
set.seed(2021)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


set.seed(2020)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") + geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,arrow = a, end_cap = circle(.07, 'inches')) + geom_node_point(color = "lightblue", size = 5) +geom_node_text(aes(label = name), vjust = 1, hjust = 1) + theme_void()

##################counting and correlating#############################

music_section_words <- music %>%
  unnest_tokens(word, reviewText) %>%
  filter(!word %in% stop_words$word)
music_section_words

library(widyr)
# count words co-occuring within sections
word_pairs <- music_section_words %>%
  pairwise_count(word, reviewerID, sort = TRUE)
word_pairs

word_pairs %>%
  filter(item1 == "guitar")

# we need to filter for at least relatively common words first
word_cors <- music_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, reviewerID, sort = TRUE)
word_cors

word_cors %>%
  filter(item1 == "sound")

word_cors %>%
  filter(item1 %in% c("sound", "music", "guitar", "play")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

set.seed(2016)
word_cors %>%
  filter(correlation > .45) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()









