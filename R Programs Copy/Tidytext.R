remove(list=ls())

text <- c("Because I could not stop for Death -", "He kindly stopped for me -",
          "The Carriage held but just Ourselves -", "and Immortality")
text
class(text)

library(dplyr)
text_df <- data_frame(line = 1:4, text = text)
text_df

library(tidytext) 
temp <- text_df %>%
  unnest_tokens(word, text)
temp

library(janeaustenr)
library(dplyr)
library(stringr)
austen_books()
original_books <- austen_books() %>% group_by(book) %>% 
  mutate(linenumber = row_number(), chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                                            ignore_case = TRUE)))) %>% ungroup()
original_books

org_books <- original_books %>%
  unnest_tokens(word, text)
org_books

library(tidytext)
tidy_books <- original_books %>% unnest_tokens(word, text) 
tidy_books

data(stop_words)
tidy_books <- tidy_books %>% anti_join(stop_words)

tidy_books %>% count(word, sort = TRUE)

library(ggplot2)
tidy_books %>%
  count(word, sort = TRUE) %>% filter(n > 700) %>%
  mutate(word = reorder(word, n)) %>% ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


library(gutenbergr)
hgwells <- gutenberg_download(c(35, 36, 5230, 159)) #Getting data from the following books; The Time Machine, The War of the Worlds, The Invisible Man, and The Island of Doctor Moreau. 
tidy_hgwells <- hgwells %>% unnest_tokens(word, text) %>% anti_join(stop_words)

tidy_hgwells %>% count(word, sort = TRUE)

bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767)) #Getting data from the following books; Jane Eyre, Wuthering Heights, The Tenant of Wildfell Hall, Villette, and Agnes Grey. 
tidy_bronte <- bronte %>% unnest_tokens(word, text) %>% anti_join(stop_words)                                                 

tidy_bronte %>% count(word, sort = TRUE)

library(tidyr)
frequency <- bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"), mutate(tidy_hgwells, author = "H.G. Wells"),
                       mutate(tidy_books, author = "Jane Austen")) %>% mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>% group_by(author) %>% mutate(proportion = n / sum(n)) %>% select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `Brontë Sisters`:`H.G. Wells`)
head(frequency)
tail(frequency)

library(scales)
# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = `Jane Austen`,
                      color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) + geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) + scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) + scale_color_gradient(limits = c(0, 0.001),
                                                                  low = "darkslategray4", high = "gray75") + facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") + labs(y = "Jane Austen", x = NULL)

cor.test(data = frequency[frequency$author == "Brontë Sisters",], ~ proportion + `Jane Austen`)
cor.test(data = frequency[frequency$author == "H.G. Wells",], ~ proportion + `Jane Austen`)

###############Amazon reviews- Example####################

remove(list=ls())

data<-read.csv("/Users/poojasengupta/Documents/Teaching/Text Analytics/Amazon cell phone review/20191226-reviews.csv", header = TRUE)
str(data)

#data_new<- head(data,n=200)
#library(tidytext) 
#data_new %>% unnest_tokens(word, body)

library(tidytext)
tidy_reviews_cellphone <- data %>% unnest_tokens(word, body) 
tidy_reviews_cellphone

data(stop_words)
tidy_reviews_cellphone <- tidy_reviews_cellphone %>% anti_join(stop_words)

tidy_reviews_cellphone %>% count(word, sort = TRUE)

library(ggplot2)
tidy_reviews_cellphone %>%
  count(word, sort = TRUE) %>% filter(n > 5500) %>%
  mutate(word = reorder(word, n)) %>% ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

data_earphone<-read.csv("/Users/poojasengupta/Documents/Teaching/Text Analytics/Amazon earphone review/AllProductReviews.csv", header = TRUE)
str(data_earphone)

library(tidytext)
tidy_reviews_earphone <- data_earphone %>% unnest_tokens(word, ReviewBody) 
tidy_reviews_earphone

data(stop_words)
tidy_reviews_earphone <- tidy_reviews_earphone %>% anti_join(stop_words)
tidy_reviews_earphone %>% count(word, sort = TRUE)

library(ggplot2)
tidy_reviews_earphone %>%
  count(word, sort = TRUE) %>% filter(n > 700) %>%
  mutate(word = reorder(word, n)) %>% ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


library(tidyr)
frequency <- bind_rows(mutate(tidy_reviews_earphone, product = "Earphone reviews"), mutate(tidy_reviews_cellphone, product = "Cellphone reviews")) %>% mutate(word = str_extract(word, "[a-z']+")) %>%
  count(product, word) %>% group_by(product) %>% mutate(proportion = n / sum(n)) %>% select(-n) %>%
  spread(product, proportion) %>%
  gather(product, proportion, `Earphone reviews`)

library(scales)
# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = `Cellphone reviews`,
                      color = abs(`Cellphone reviews` - proportion))) +
  geom_abline(color = "gray40", lty = 10) +
  geom_jitter(alpha = 0.1, size = 1, width = 0.3, height = 0.3) + geom_text(aes(label = word), check_overlap = TRUE) + scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) + scale_color_gradient(limits = c(0, 0.001),
                                                                  low = "darkslategray4", high = "gray75") + facet_wrap(~product, ncol = 1) +
  theme(legend.position="none") + labs(y = "Cellphone reviews", x = NULL)

cor.test(data = frequency[frequency$product == "Earphone reviews",], ~ proportion + `Cellphone reviews`)


###########################################Apparel####################################

data_apparel<-read.csv("/Users/poojasengupta/Documents/Teaching/Text Analytics/Clothing-Reviews.csv", header = TRUE)
str(data_apparel)

library(tidytext)
tidy_reviews_apparel <- data_apparel %>% unnest_tokens(word, review_text) 
tidy_reviews_apparel

data(stop_words)
tidy_reviews_apparel <- tidy_reviews_apparel %>% anti_join(stop_words)
tidy_reviews_apparel %>% count(word, sort = TRUE)

library(ggplot2)
tidy_reviews_apparel %>%
  count(word, sort = TRUE) %>% filter(n > 6000) %>%
  mutate(word = reorder(word, n)) %>% ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


library(tidyr)
frequency <- bind_rows(mutate(tidy_reviews_apparel, product = "Apparel reviews"), mutate(tidy_reviews_cellphone, product = "Cellphone reviews")) %>% mutate(word = str_extract(word, "[a-z']+")) %>%
  count(product, word) %>% group_by(product) %>% mutate(proportion = n / sum(n)) %>% select(-n) %>%
  spread(product, proportion) %>%
  gather(product, proportion, `Apparel reviews`)

library(scales)
# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = `Cellphone reviews`,
                      color = abs(`Cellphone reviews` - proportion))) +
  geom_abline(color = "gray40", lty = 10) +
  geom_jitter(alpha = 0.1, size = 1, width = 0.3, height = 0.3) + geom_text(aes(label = word), check_overlap = TRUE) + scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) + scale_color_gradient(limits = c(0, 0.001),
                                                                  low = "darkslategray4", high = "gray75") + facet_wrap(~product, ncol = 1) +
  theme(legend.position="none") + labs(y = "Cellphone reviews", x = NULL)

cor.test(data = frequency[frequency$product == "Apparel reviews",], ~ proportion + `Cellphone reviews`)




