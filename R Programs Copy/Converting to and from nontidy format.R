remove(list=ls())

library(tm)
data("AssociatedPress", package = "topicmodels")
AssociatedPress

terms <- Terms(AssociatedPress) 
head(terms)

####################Converting to tidy format#############################

library(dplyr)
library(tidytext)
ap_td <- tidy(AssociatedPress) 
ap_td
tail(ap_td)

########################Sentiment analysis on tidy data###################

ap_sentiments <- ap_td %>% inner_join(get_sentiments("bing"), by = c(term = "word"))
ap_sentiments

library(ggplot2)
ap_sentiments %>%
  count(sentiment, term, wt = count) %>%
  ungroup() %>%
  filter(n >= 200) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>% mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) + geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") +
  coord_flip()

######################Tidy from dfm in quanteda package#########################

library(methods)
data("data_corpus_inaugural", package = "quanteda")
inaug_dfm <- quanteda::dfm(data_corpus_inaugural, verbose = FALSE) 
inaug_dfm
inaug_td <- tidy(inaug_dfm) 
inaug_td

library(tidyverse)
inaug_tf_idf <- inaug_td %>% bind_tf_idf(term, document, count) %>% arrange(desc(tf_idf))
inaug_tf_idf

library(tidyr)
year_term_counts <- inaug_td %>%
  extract(document, "year", "(\\d+)", convert = TRUE) %>% complete(year, term, fill = list(count = 0)) %>% group_by(year) %>%
  mutate(year_total = sum(count))
year_term_counts

# A list of words to check on can be arrived at by chcking the TF-IDF scores periodwise (or) by checking the sentiment & listing top "n" positive & negative words as required...
year_term_counts %>%
  filter(term %in% c("god", "america", "foreign",
                     "union", "constitution", "freedom")) %>% ggplot(aes(year, count / year_total)) + geom_point() + geom_smooth() + facet_wrap(~ term, scales = "free_y") + scale_y_continuous(labels = scales::percent_format()) + ylab("% frequency of word in inaugural address")

#####################Casting Tidy Text Data into a Matrix#######################

ap_td %>%
  cast_dtm(document, term, count)

ap_td %>%
  cast_dfm(term, document, count)

library(Matrix)
# cast into a Matrix object
m <- ap_td %>% cast_sparse(document, term, count)
class(m)
dim(m)

#########################Example: Jane austen###################################

library(janeaustenr)
austen_dtm <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word) %>%
  cast_dtm(book, word, n)
austen_dtm

#######################Tidying Corpus Objects with Metadata#####################

library(tm)
data("acq")
acq[1]

# first document
acq[[1]]

acq_td <- tidy(acq) 
acq_td

acq_tokens <- acq_td %>% select(-places) %>% unnest_tokens(word, text) %>% anti_join(stop_words, by = "word")
# most common words
acq_tokens %>% count(word, sort = TRUE)

# tf-idf
acq_token_tfidf <- acq_tokens %>%
  count(id, word) %>% bind_tf_idf(word, id, n) %>% arrange(desc(tf_idf))

#############################Example 1#####################################


# install.packages("pdftools")
library(pdftools)
library(tidyverse)
library(tidytext)
library(tm)

setwd("/Users/poojasengupta/Documents/Teaching/Text Analytics/Mahindra & Mahindra")
files <- list.files(pattern = "pdf$")

reports <- lapply(files, pdf_text)
length(reports)

lapply(reports, length)
reports

library(tm)
corp <- Corpus(URISource(files),readerControl <- list(reader = readPDF))
corp[1]

#library(tm)
# first document
corp[[1]]

corp_td <- tidy(corp) 
corp_td

corp_tokens <- corp_td %>% unnest_tokens(word, text) %>% anti_join(stop_words, by = "word")
# most common words
corp_tokens %>% count(word, sort = TRUE)

# tf-idf
corp_tokens %>%
  count(id, word) %>% bind_tf_idf(word, id, n) %>% arrange(desc(tf_idf))

corp_tokens %>%
  count(id, word) %>% bind_tf_idf(word, id, n) %>% arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% group_by(id) %>%
  top_n(4) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = id)) + geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~id, ncol = 2, scales = "free") + coord_flip()

###########################Example 2#################################

data <- read.csv("C:/Users/Shreyansh Mohanty/Desktop/IIM Ranchi MBA-BA Files/Term-6/TAUR/R Programs/Datasets/train-balanced-sarcasm.csv", header = TRUE)
data<-filter(data, subreddit == c("politics", "movies", "FIFA"))
head(data)
str(data)


library(tm)
library(quanteda)

corp <- corpus(data, text_field = "comment")
print(corp)

corp_td <- tidy(corp) 
corp_td

corp_tokens <- corp_td %>% unnest_tokens(word, text) %>% anti_join(stop_words, by = "word")
# most common words
corp_tokens %>% count(word, sort = TRUE)

# Removing custom stop-words also required...

# tf-idf
corp_tokens %>%
  count(subreddit , word) %>% bind_tf_idf(word, subreddit, n) %>% arrange(desc(tf_idf))

corp_tokens %>%
  count(subreddit, word) %>% bind_tf_idf(word, subreddit, n) %>% arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% group_by(subreddit) %>%
  top_n(5) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = subreddit)) + geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~subreddit, ncol = 2, scales = "free") + coord_flip()



##################tidy to DTM############################

data_dtm <- data %>%
  unnest_tokens(word, comment) %>% anti_join(stop_words, by = "word") %>%
  count(subreddit, word) %>%
  cast_dtm(subreddit, word, n)
data_dtm

dtm_tfidf <- removeSparseTerms(data_dtm, 0.50)
dtm_tfidf


freq = data.frame(sort(colSums(as.matrix(dtm_tfidf)), decreasing=TRUE))
library(wordcloud)
wordcloud(rownames(freq), freq[,1], max.words=100, colors=brewer.pal(1, "Dark2"))






