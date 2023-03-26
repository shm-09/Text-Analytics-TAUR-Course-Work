#Global environment clearance
remove(list=ls())

#Loading libraries
library(xlsx)
library(dplyr)
library(wordcloud)
library(reshape2)
library(ggplot2)
library(tidyr)
library(tidytext)
library(tidyverse)
library(igraph)
library(NLP)
library(stringr)
#library(MASS)

#Importing Dataset
snapchat <- read.csv("Snapchat_app_store_reviews.csv")
str(snapchat)

#Data Pre-processing

snapchat_processed <- snapchat %>%
  mutate(review_clean = tolower(review)) %>%
  mutate(review_clean = str_replace_all(review_clean, "\\s+", " ")) %>%
  mutate(review_clean = str_remove_all(review_clean, "[[:punct:]]")) %>%
  mutate(review_clean = str_remove_all(review_clean, "[[:digit:]]")) %>%
  mutate(review_clean = str_remove_all(review_clean, "[[:emoji:]]"))

#Basic EDA

#Descriptive summary of rating
summary(snapchat_processed$rating)

#Histogram
snapchat_processed %>%
  dplyr::select(-X) %>%
  select_if(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) + 
  geom_histogram() + 
  facet_wrap(~ key, scales = "free") +
  labs(title = "Ratings") +
  ylab("Cumulative Frequency") + xlab("Values") +
  theme(plot.title = element_text(hjust = 0.5))

library(lubridate)
snapchat_processed$date_new <- format(as.Date(snapchat_processed$date, format = "%m/%d/%y %H:%M"), "%d-%m-%Y")
snapchat_processed$year <- format(as.Date(snapchat_processed$date, format = "%m/%d/%y %H:%M"), "%Y")

table(snapchat_processed$year, snapchat_processed$rating)

"
df <- snapchat_processed %>% 
  group_by(year, rating) %>%
  summarise(freq= n(), .groups = 'drop')

df1 <- snapchat_processed %>% 
  group_by(year) %>%
  summarise(freq= n(), .groups = 'drop')

df <- left_join(df, df1, by = year)
df <- mutate(df, proportion = freq.x/freq.y)

df_spread <- spread(df, key = rating, value = proportion)
df_spread
"

#to check for null values
any(is.na(snapchat_processed$year))

#library(openxlsx)
#write.xlsx(snapchat_processed, "snap_test.xlsx", rowNames = FALSE)

#Wordcloud

custom_stop_words <- bind_rows(data_frame(word = c("snapchat","app","ive","im"), lexicon = c("custom")),stop_words)
custom_stop_words

#Most frequent words cloud
library(wordcloud)
snapchat_processed %>% unnest_tokens(word, review_clean) %>%
  anti_join(custom_stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, random.order = FALSE, max.words = 100, colors = brewer.pal(8, "Dark2")))

#Most frequent words cloud for rating = 1
library(wordcloud)
snapchat_processed %>% filter(rating == 1) %>% unnest_tokens(word, review_clean) %>%
  anti_join(custom_stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, random.order = FALSE, max.words = 100, colors = brewer.pal(8, "Dark2"))) %>%
  title("Wordcloud of Rating = 1")

#Most frequent words cloud for rating = 2
library(wordcloud)
snapchat_processed %>% filter(rating == 2) %>% unnest_tokens(word, review_clean) %>%
  anti_join(custom_stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, random.order = FALSE, max.words = 80, colors = brewer.pal(8, "Dark2"))) %>%
  title("Wordcloud of Rating = 2")

#Most frequent words cloud for rating = 3
library(wordcloud)
snapchat_processed %>% filter(rating == 3) %>% unnest_tokens(word, review_clean) %>%
  anti_join(custom_stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, random.order = FALSE, max.words = 70, colors = brewer.pal(8, "Dark2"))) %>%
  title("Wordcloud of Rating = 3")

#Most frequent words cloud for rating = 4
library(wordcloud)
snapchat_processed %>% filter(rating == 4) %>% unnest_tokens(word, review_clean) %>%
  anti_join(custom_stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, random.order = FALSE, max.words = 80, colors = brewer.pal(8, "Dark2"))) %>%
  title("Wordcloud of Rating = 4")

#Most frequent words cloud for rating = 5
library(wordcloud)
snapchat_processed %>% filter(rating == 5) %>% unnest_tokens(word, review_clean) %>%
  anti_join(custom_stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, random.order = FALSE, max.words = 100, colors = brewer.pal(8, "Dark2"))) %>%
  title("Wordcloud of Rating = 5")

#Most frequent words sentiment wise
library(reshape2)
snapchat_processed %>% unnest_tokens(word, review_clean) %>% anti_join(custom_stop_words) %>%
  inner_join(get_sentiments("nrc"), multiple = "all") %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% comparison.cloud(#colors = c("gray20", "gray80"),
    max.words = 100)

######### Word changing over years ##################

library(tidyverse)
library(tidytext)
library(lubridate)

# assuming your data frame is named `snapchat_reviews`
# convert date to a proper date format
snapchat_processed <- snapchat_processed %>% 
  mutate(date1 = dmy(date_new))

# create a document-term matrix
library(tidytext)
library(dplyr)
library(tidyr)
library(stringr)

dtm10 <- snapchat_processed %>% 
  unnest_tokens(word, review_clean) %>% 
  anti_join(custom_stop_words) %>% 
  mutate(year = year(date1)) %>% 
  group_by(year) %>% 
  count(word, sort = TRUE) %>% 
  #slice_max(n = 10, order_by = n) %>% 
  ungroup() %>% 
  bind_tf_idf(term = word, document = year, n)

# plot the top 10 words based on tf-idfs for each rating and year
dtm10 %>%
  group_by(year) %>%
  top_n(10, tf_idf) %>%
  ggplot(aes(reorder_within(word, tf_idf, year), tf_idf, fill = year)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ year, scales = "free_x", nrow = 1) +
  labs(title = "Top 10 Words by TF-IDF for Each Year",
       subtitle = "Excluding Stop Words",
       x = NULL, y = "TF-IDF Score") +
  scale_x_reordered() +
  coord_flip()

#####################################################

#Sentiment mapping

sentiments <- snapchat_processed %>%
  unnest_tokens(word, review_clean) %>% anti_join(custom_stop_words) %>%
  inner_join(get_sentiments("nrc"), by = "word")

nrc_song <- snapchat_processed %>%
  unnest_tokens(word, review_clean, token = "words") %>% anti_join(custom_stop_words) %>%
  inner_join(get_sentiments("nrc")) %>% count(word, sentiment, sort = TRUE) %>% ungroup()

#Sentiment Matrix creation
sentiment_matrix <- sentiments %>% 
  group_by(userName, rating, sentiment) %>% 
  summarise(sentiment_count = n()) %>% 
  ungroup() %>% spread(sentiment, sentiment_count, fill = 0)

sentiment_matrix$rating <- as.factor(sentiment_matrix$rating)

sentiment_matrix <- sentiment_matrix %>% 
  group_by(userName, rating) %>% 
  summarise(across(everything(), ~sum(.))) %>% 
  ungroup() %>% 
  mutate(total = rowSums(select_if(., is.numeric)))

sentiment_matrix <- sentiment_matrix %>% 
  mutate(across(.cols = where(is.numeric), ~./total))

sentiment_matrix

#TF-IDF

data1 <- snapchat_processed %>% 
  unnest_tokens(word, review_clean, token = "words") %>% 
  count(rating, word, sort = TRUE) %>% ungroup()

data1 <- data1 %>% anti_join(custom_stop_words)

total_words <- data1 %>% group_by(rating) %>% summarise(total = sum(n))
data1 <- left_join(data1, total_words)
data1

ggplot(data1, aes(n/total, fill = rating)) + geom_histogram(show.legend = FALSE) + xlim(NA, 0.0009) +
  facet_wrap(~rating, ncol = 2, scales = "free_y")

############# TF-IDF basis Rating ###############

freq_by_rank <- data1 %>% group_by(rating) %>% 
  mutate(rank = row_number(),`term frequency` = n/total) 
freq_by_rank

freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color = rating)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = TRUE) + 
  scale_x_log10() +
  scale_y_log10()

"
Observation: 
Term frequency-Rank graph shows:
    1. Curve spans from rank 1-10K+. 
    2. Common words are ranked in the range 1-1000 indication frequent usage of such words.
    3. For each genre, curve starts to scatter after rank 1000 attributed to the presence of uncommon/ infrequent words 
"

rank_subset <- freq_by_rank %>% filter(rank < 1000, rank > 10)
out <- lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)
summary(out)

"
Interpretation: 
    1. Model significance: Significant at 5% level of significance since p-value: < 2e-16 (<5%)
    2. Coeff. significance: Significant at 5% level of significance since p-value: < 2e-16 (<5%)
    3. Zipf's law verified i.e. TF inversely proportional to rank, since slope is negative and close to 1. 
"

#TF-rank plot
freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color = rating)) +
  geom_abline(intercept = out$coefficients[1], slope = out$coefficients[2], color = "gray50", linetype = 2) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = TRUE) +
  scale_x_log10() +
  scale_y_log10()

#Binding IDF & TF-IDF to data1
data1 <- data1 %>% bind_tf_idf(word, rating, n)
data1
data1 %>% select(-total) %>% arrange(desc(tf_idf))
data1 %>% select(-total) %>% arrange(tf_idf)

#Rating wise TF-IDF plot
data1 %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% group_by(rating) %>%
  top_n(5) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = rating)) + geom_col(show.legend = TRUE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~rating, ncol = 2, scales = "free") + coord_flip()

#Contribution to sentiment graph

nrc_song <- snapchat_processed %>%
  unnest_tokens(word, review_clean, token = "words") %>% anti_join(custom_stop_words) %>%
  inner_join(get_sentiments("nrc")) %>% count(word, sentiment, sort = TRUE) %>% ungroup()

nrc_song %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>% ggplot(aes(word, n, fill = sentiment)) + 
  geom_col(show.legend = FALSE) + facet_wrap(~sentiment, scales = "free_y") + 
  labs(y = "Contribution to sentiment",x=NULL)+ coord_flip()

######################################################################################

#LDA with rating 5

# set a seed so that the output of the model is predictable
library(topicmodels)

dtm5 <- sentiments %>% filter(rating == 5) %>%  # split text into words
  anti_join(custom_stop_words) %>%  # remove stop words
  count(userName, word) %>%  # count the frequency of each word in each document
  cast_dtm(document = userName, term = word, value = n)

# set a seed so that the output of the model is predictable
ap_lda <- LDA(dtm5, k = 4, control = list(seed = 1234))
ap_lda

tidy(ap_lda)

library(tidytext)
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

library(ggplot2)
library(dplyr)
ap_top_terms <- ap_topics %>%
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

library(tidyr)
beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))
beta_spread


beta_spread %>% 
  filter(log_ratio > 8) %>% 
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) + geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") + 
  coord_flip()

ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents

##############################################################################

#LDA with rating 4

# set a seed so that the output of the model is predictable
library(topicmodels)

dtm4 <- sentiments %>% filter(rating == 4) %>%  # split text into words
  anti_join(custom_stop_words) %>%  # remove stop words
  count(userName, word) %>%  # count the frequency of each word in each document
  cast_dtm(document = userName, term = word, value = n)

# set a seed so that the output of the model is predictable
ap_lda <- LDA(dtm4, k = 3, control = list(seed = 1234))
ap_lda

tidy(ap_lda)

library(tidytext)
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

library(ggplot2)
library(dplyr)
ap_top_terms <- ap_topics %>%
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

library(tidyr)
beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))
beta_spread


beta_spread %>% 
  filter(log_ratio > 8) %>% 
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) + geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") + 
  coord_flip()

ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents

##############################################################################

#LDA with rating 3

# set a seed so that the output of the model is predictable
library(topicmodels)

dtm3 <- sentiments %>% filter(rating == 3) %>%  # split text into words
  anti_join(custom_stop_words) %>%  # remove stop words
  count(userName, word) %>%  # count the frequency of each word in each document
  cast_dtm(document = userName, term = word, value = n)

# set a seed so that the output of the model is predictable
ap_lda <- LDA(dtm3, k = 2, control = list(seed = 1234))
ap_lda

tidy(ap_lda)

library(tidytext)
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

library(ggplot2)
library(dplyr)
ap_top_terms <- ap_topics %>%
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

library(tidyr)
beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))
beta_spread


beta_spread %>% 
  filter(log_ratio > 8) %>% 
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) + geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") + 
  coord_flip()

ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents

##############################################################################

#LDA with rating 2

# set a seed so that the output of the model is predictable
library(topicmodels)

dtm2 <- sentiments %>% filter(rating == 2) %>%  # split text into words
  anti_join(custom_stop_words) %>%  # remove stop words
  count(userName, word) %>%  # count the frequency of each word in each document
  cast_dtm(document = userName, term = word, value = n)

# set a seed so that the output of the model is predictable
ap_lda <- LDA(dtm2, k = 2, control = list(seed = 1234))
ap_lda

tidy(ap_lda)

library(tidytext)
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

library(ggplot2)
library(dplyr)
ap_top_terms <- ap_topics %>%
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

library(tidyr)
beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))
beta_spread


beta_spread %>% 
  filter(log_ratio > 8) %>% 
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) + geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") + 
  coord_flip()

ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents

###############################################################################

#LDA with rating 1

# set a seed so that the output of the model is predictable
library(topicmodels)

dtm1 <- sentiments %>% filter(rating == 1) %>%  # split text into words
  anti_join(custom_stop_words) %>%  # remove stop words
  count(userName, word) %>%  # count the frequency of each word in each document
  cast_dtm(document = userName, term = word, value = n)

# set a seed so that the output of the model is predictable
ap_lda <- LDA(dtm1, k = 2, control = list(seed = 1234))
ap_lda

tidy(ap_lda)

library(tidytext)
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

library(ggplot2)
library(dplyr)
ap_top_terms <- ap_topics %>%
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

library(tidyr)
beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))
beta_spread


beta_spread %>% 
  filter(log_ratio > 8) %>% 
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) + geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") + 
  coord_flip()

ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents

##############################################################################

"
tidy(dtm) %>%
  filter(document == 6) %>%
  arrange(desc(count))
"

#Multinomial Regression | Dataframe: sentiment_matrix | Rating vs sentiment

library(nnet)

sentiment_matrix$rating <- relevel(as.factor(sentiment_matrix$rating), ref = "1")
multinom_model <- multinom(rating ~ anger + anticipation + disgust + fear + 
                           joy + negative + positive + sadness + surprise + trust, sentiment_matrix)
summary(multinom_model)

summary1 <- summary(multinom_model)

#p-values
pt(abs(summary1$coefficients / summary1$standard.errors), df=nrow(sentiment_matrix)-10,
   lower=FALSE)