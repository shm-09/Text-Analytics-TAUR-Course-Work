
library(xlsx)
data<-read.xlsx("/Users/poojasengupta/Documents/Teaching/Text Analytics/rotten_tomatoes_sentiment.xlsx", 1)
str(data)

data$top_critic<-as.factor(data$top_critic)
data$review_type<-as.factor(data$review_type)

model<-lm(polarity~top_critic + review_type + Review_scores + subjectivity + 
            Review_scores * subjectivity, data = data)
summary(model)
