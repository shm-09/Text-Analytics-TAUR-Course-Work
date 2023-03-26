remove(list=ls())

library(topicmodels)
data("AssociatedPress")
AssociatedPress

# set a seed so that the output of the model is predictable
ap_lda <- LDA(AssociatedPress, k = 3, control = list(seed = 1234))
ap_lda

# set a seed so that the output of the model is predictable
ap_lda_k2 <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))
ap_lda_k2


#tidy(ap_lda)

library(tidytext)
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

ap_topics_k2 <- tidy(ap_lda_k2, matrix = "beta")
ap_topics_k2


library(ggplot2)
library(dplyr)
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms_k2 <- ap_topics_k2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

ap_top_terms_k2 %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()



library(tidyr)
beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic3 > .001) %>%
  mutate(log_ratio = log2(topic3 / topic1))
beta_spread

beta_spread_k2 <- ap_topics_k2 %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))
beta_spread_k2


beta_spread %>% 
  filter(log_ratio > 8) %>% 
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) + geom_bar(stat = "identity") +
  ylab("Contribution to sentiment - k3") + 
  coord_flip()

beta_spread_k2 %>% 
  filter(log_ratio > 8) %>% 
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) + geom_bar(stat = "identity") +
  ylab("Contribution to sentiment - k2") + 
  coord_flip()

######################Document topic probability#############################

ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents

ap_documents_k2 <- tidy(ap_lda_k2, matrix = "gamma")
ap_documents_k2

tidy(AssociatedPress) %>%
  filter(document ==6) %>%
  arrange(desc(count))


##############################EXample:Library Heist #######################################
remove(list=ls())

titles <- c("Twenty Thousand Leagues under the Sea", "The War of the Worlds","Pride and Prejudice", "Great Expectations")
library(gutenbergr)
books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title", mirror = "http://mirrors.xmission.com/gutenberg/")

library(stringr)
# divide into documents, each representing one chapter
reg <- regex("^chapter ", ignore_case = TRUE)
by_chapter <- books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(text, reg))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

# split into words
by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text)

# find document-word counts
word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>%
  count(document, word, sort = TRUE) %>%
  ungroup()
word_counts


############################LDA on chapters###############################

chapters_dtm <- word_counts %>%
  cast_dtm(document, word, n)
chapters_dtm

chapters_lda <- LDA(chapters_dtm, k = 4, control = list(seed = 1234))
chapters_lda

chapter_topics <- tidy(chapters_lda, matrix = "beta")
chapter_topics

top_terms <- chapter_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms

library(ggplot2)
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


#########################Pre-document classification##########################

chapters_gamma <- tidy(chapters_lda, matrix = "gamma")
chapters_gamma

chapters_gamma <- chapters_gamma %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE)
chapters_gamma

# reorder titles in order of topic 1, topic 2, etc. before plotting
chapters_gamma %>%
  mutate(title = reorder(title, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title)

chapter_classifications <- chapters_gamma %>%
  group_by(title, chapter) %>%
  top_n(1, gamma) %>%
  ungroup()
chapter_classifications

#########################################################################

book_topics <- chapter_classifications %>%
  count(title, topic) %>%
  group_by(title) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consensus = title, topic)

chapter_classifications %>%
  inner_join(book_topics, by = "topic") %>%
  filter(title != consensus)

###############################By-Word Assignments: augment############

assignments <- augment(chapters_lda, data = chapters_dtm)
assignments

assignments <- assignments %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE) %>%
  inner_join(book_topics, by = c(".topic" = "topic"))
assignments

library(dplyr)
library(ggplot2)
assignments %>%
  count(title, consensus, wt = count) %>%
  group_by(title) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(consensus, title, fill = percent)) +
  geom_tile() +
  scale_fill_gradient2(high = "red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Book words were assigned to",
       y = "Book words came from",
       fill = "% of assignments")

#########What were the most commonly mistaken words?###############

wrong_words <- assignments %>%
  filter(title != consensus)

wrong_words %>%
  count(title, consensus, term, wt = count) %>%
  ungroup() %>%
  arrange(desc(n))

word_counts %>%
  filter(word == "pip")

############################Alternative LDA Implementations#############

library(mallet)
# create a vector with one string per chapter
collapsed <- by_chapter_word %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word = str_replace(word, "'", "")) %>%
  group_by(document) %>%
  summarize(text = paste(word, collapse = " "))
# create an empty file of "stop words"
file.create(empty_file <- tempfile())
docs <- mallet.import(collapsed$document, collapsed$text, empty_file)
mallet_model <- MalletLDA(num.topics = 4)
mallet_model$loadDocuments(docs)
mallet_model$train(100)

# word-topic pairs
tidy(mallet_model)
# document-topic pairs
tidy(mallet_model, matrix = "gamma")
# column needs to be named "term" for "augment"
term_counts <- rename(word_counts, term = word)
augment(mallet_model, term_counts)

############################################Example########################







