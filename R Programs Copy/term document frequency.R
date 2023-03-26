remove(list=ls())

library(dplyr)
library(janeaustenr)
library(tidytext)
book_words <- austen_books() %>% unnest_tokens(word, text) %>% count(book, word, sort = TRUE) %>% ungroup()
data(stop_words)
book_words <- book_words %>% anti_join(stop_words)

total_words <- book_words %>% group_by(book) %>% summarise(total = sum(n))
book_words <- left_join(book_words, total_words)
book_words


library(ggplot2)
ggplot(book_words, aes(n/total, fill = book)) + geom_histogram(show.legend = FALSE) + xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")

#####################################Zipf's law################################

freq_by_rank <- book_words %>% group_by(book) %>% 
  mutate(rank = row_number(),`term frequency` = n/total) 
freq_by_rank

freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color = book)) + geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + scale_x_log10() + 
  scale_y_log10()

rank_subset <- freq_by_rank %>% filter(rank < 1000, rank > 10)
lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

# Log transformation -> As expected relationship between rank & term frequency is inverse and the scales are different 
# --> i.e term freq = 1/rank

freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color = book)) +
  geom_abline(intercept = -1.65, slope = -0.63, color = "gray50", linetype = 2) + geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()

############TF-IDF##############
# TF -> Term Frequency
# IDF -> Inverse Document Frequency

book_words <- book_words %>% bind_tf_idf(word, book, n)
book_words
book_words %>% select(-total) %>% arrange(desc(tf_idf))
book_words %>% select(-total) %>% arrange(tf_idf)


book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% group_by(book) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = book)) + geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") + coord_flip()

######################### Hotel review #######################

data <- read.csv("C:/Users/Shreyansh Mohanty/Desktop/IIM Ranchi MBA-BA Files/Term-6/TAUR/R Programs/Datasets/Datafiniti_Hotel_Reviews.csv", header = TRUE)
str(data)

library(dplyr)
library(tidytext)

#data<-head(data,n=10)

data_tidy <- data %>% unnest_tokens(word, reviews.text)%>% count(province, word, sort = TRUE) %>% ungroup()
data_tidy <- data_tidy %>% anti_join(stop_words)
total_data_words <- data_tidy %>% group_by(province) %>% summarise(total = sum(n))
data_tidy <- left_join(data_tidy, total_data_words)
data_tidy

# Introduce code to remove some custom stop words here ---

library(ggplot2)
#ggplot(data_tidy, aes(n/total, fill = Department.Name)) + geom_histogram(show.legend = FALSE)
ggplot(data_tidy, aes(n/total, fill = province)) + geom_histogram(show.legend = FALSE) + xlim(NA, 0.30) +
  facet_wrap(~province, ncol = 3, scales = "free_y")

freq_by_rank <- data_tidy %>% group_by(province) %>% 
  mutate(rank = row_number(),`term frequency` = n/total) 
freq_by_rank

freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color = province)) + geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + scale_x_log10() +
  scale_y_log10()

rank_subset <- freq_by_rank %>% filter(rank < 500, rank > 10)
lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color = province)) +
  geom_abline(intercept = -1.259, slope = -0.74, color = "gray50", linetype = 2) + geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()

############TF-IDF##############

data_tidy <- data_tidy %>% bind_tf_idf(word, province, n)
data_tidy
data_tidy %>% select(-total) %>% arrange(desc(tf_idf))

data_tidy %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% group_by(province) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = province)) + geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~province, ncol = 2, scales = "free") + coord_flip()

# Filtering 2 states & showing TF-IDF plot...

data_tidy %>%
  filter(province == c("CA", "VA")) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% group_by(province) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = province)) + geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~province, ncol = 2, scales = "free") + coord_flip()


####################################New method##################################

data <- read.csv("C:/Users/Shreyansh Mohanty/Desktop/IIM Ranchi MBA-BA Files/Term-6/TAUR/R Programs/Datasets/Datafiniti_Hotel_Reviews.csv", header = TRUE)
str(data)

library(tm)
library(SnowballC)
library(wordcloud)

review_corpus <- Corpus(VectorSource(data$reviews.text))
review_corpus <- tm_map(review_corpus, content_transformer(tolower))
review_corpus <- tm_map(review_corpus, removeNumbers)
review_corpus <- tm_map(review_corpus, removePunctuation)
review_corpus <- tm_map(review_corpus, removeWords, c("the", "and", stopwords("english")))
review_corpus <- tm_map(review_corpus, stripWhitespace)

inspect(review_corpus[1])
review_dtm <- DocumentTermMatrix(review_corpus)
review_dtm

inspect(review_dtm[600:605, 500:505])

review_dtm <- removeSparseTerms(review_dtm, 0.95)
review_dtm

inspect(review_dtm[1,1:20])
class(review_dtm)

findFreqTerms(review_dtm, 1000)
freq <- data.frame(sort(colSums(as.matrix(review_dtm)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=50, colors=brewer.pal(1, "Dark2"))

review_dtm_tfidf <- DocumentTermMatrix(review_corpus, control = list(weighting = weightTfIdf))
review_dtm_tfidf <- removeSparseTerms(review_dtm_tfidf, 0.95)
review_dtm_tfidf

# The first document
inspect(review_dtm_tfidf[1,1:20])


freq = data.frame(sort(colSums(as.matrix(review_dtm_tfidf)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=100, colors=brewer.pal(1, "Dark2"))

#############################Mahindra and Mahindra##############################

# install.packages("pdftools")
library(pdftools)
library(tidyverse)
library(tidytext)
library(tm)

setwd("/Users/poojasengupta/Documents/Teaching/Text Analytics")
files <- list.files(pattern = "pdf$")

reports <- lapply(files, pdf_text)
length(reports)

lapply(reports, length)
reports

library(tm)
corp <- Corpus(URISource(files),readerControl <- list(reader = readPDF))

inspect(corp[1])
review_dtm <- DocumentTermMatrix(corp)
review_dtm

inspect(review_dtm[1:5, 1:5])

review_dtm <- removeSparseTerms(review_dtm, 0.65)
review_dtm

inspect(review_dtm[1,1:5])

findFreqTerms(review_dtm, 100)
freq <- data.frame(sort(colSums(as.matrix(review_dtm)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=50, colors=brewer.pal(1, "Dark2"))

review_dtm_tfidf <- DocumentTermMatrix(corp, control = list(weighting = weightTfIdf))
review_dtm_tfidf <- removeSparseTerms(review_dtm_tfidf, 0.65)
review_dtm_tfidf

# The first document
inspect(review_dtm_tfidf[1,1:5])


freq = data.frame(sort(colSums(as.matrix(review_dtm_tfidf)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=100, colors=brewer.pal(1, "Dark2"))



##############################################







