remove(list=ls())

library(xlsx)
library(tidytext)
library(textdata)
library(tm)
library(dplyr)
library(purrr)
library(ggplot2)
library(ggcorrplot)


data <- read.xlsx("C:/Users/Shreyansh Mohanty/Desktop/IIM Ranchi MBA-BA Files/Term-6/TAUR/R Programs/Case Study Datasets/ISKCON/ISKCON.xlsx", sheetIndex = 3)
str(data)

data <- mutate(data, D1 = case_when(data$SOURCE == 'Google + HK HILL' ~ 1,TRUE ~ 0),
               D2 = case_when(data$SOURCE == 'Google + VK HILL' ~ 1,TRUE ~ 0),
               D3 = case_when(data$SOURCE == 'Facebook' ~ 1,TRUE ~ 0))


model1<-lm(REVIEW.RATING ~ D1 + D2 + D3, data = data)
summary(model1)

model2 <- lm(REVIEW.RATING ~ SOURCE, data = data)
summary(model2)


data2 <- read.xlsx("C:/Users/Shreyansh Mohanty/Desktop/IIM Ranchi MBA-BA Files/Term-6/TAUR/R Programs/Case Study Datasets/ISKCON/IMB767-XLS-ENG.xlsx", sheetIndex = 3)
str(data)

model3 <- lm(REVIEW.RATING ~ Weekday.Weekend, data = data2)
summary(model3)

model4 <- lm(REVIEW.RATING ~ SOURCE + Weekday + (Weekday*SOURCE), data = data2)
summary(model4)

model5 <- lm(REVIEW.RATING ~ Weekday.Weekend + SOURCE + (Weekday.Weekend*SOURCE), data = data2)
summary(model5)


cross_table <- table(data$REVIEW.TYPE, data$SOURCE)
print(cross_table)
chisq.test(cross_table)

#### Multinomial Logistic REgression...
library(foreign)
library(nnet)
# data2 <- relevel(ml$prog, ref = "academic")
# mods= ~ relevel(factor(group), ref="b")
data2$REVIEW.TYPE <- relevel(as.factor(data2$REVIEW.TYPE), ref = "POSITIVE")
test <- multinom(REVIEW.TYPE ~ SOURCE, data = data2)
summary(test)


#### Sentiment Analysis... TO DO
#### Word Cloud... TO DO
#### Topic Modeling LDA... TO DO