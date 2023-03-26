remove(list=ls())

library(xlsx)
library(tidytext)
library(textdata)
library(tm)
library(dplyr)
library(purrr)
library(ggplot2)
library(ggcorrplot)


data <- read.xlsx("C:/Users/Shreyansh Mohanty/Desktop/IIM Ranchi MBA-BA Files/Term-6/TAUR/R Programs/Case Study Datasets/AirBnb/M-0897X2 paris v2.xlsx", sheetIndex = 1)
str(data)

hist(data$reviews)
hist(data$savwish)

"
model<-lm(reviews~rating + price + accommodates + extpeop + min_stay + sentiment + secdep + cleanfee + weekfee + monthfee + bedroom + bathroom + beds, data = data)
summary(model)

model2<-lm(savwish~rating + price + accommodates + extpeop + min_stay + sentiment + secdep + cleanfee + weekfee + monthfee + bedroom + bathroom + beds, data = data)
summary(model2)
"

model3 <- lm(logreviews~rating + price + accommodates + extpeop + min_stay + sentiment + secdep + cleanfee + weekfee + monthfee + bedroom + bathroom + beds, data = data)
summary(model3)

model4 <- lm(logsavwish~rating + price + accommodates + extpeop + min_stay + sentiment + secdep + cleanfee + weekfee + monthfee + bedroom + bathroom + beds, data = data)
summary(model4)





# Fitting Models for Miami dataset...
data2 <- read.xlsx("C:/Users/Shreyansh Mohanty/Desktop/IIM Ranchi MBA-BA Files/Term-6/TAUR/R Programs/Case Study Datasets/AirBnb/M-0897X1-week9 miami v2.xlsx", sheetIndex = 5)
str(data2)

hist(data2$reviews)
hist(data2$savwish)
"
modelm1<-lm(reviews~rating + price + accommodates + extpeop + min_stay + sentiment + secdep + cleanfee + weekfee + monthfee + bedroom + bathroom + beds, data = data2)
summary(modelm1)

modelm2<-lm(savwish~rating + price + accommodates + extpeop + min_stay + sentiment + secdep + cleanfee + weekfee + monthfee + bedroom + bathroom + beds, data = data2)
summary(modelm2)
"
modelm3 <- lm(logreviews~ rating + price + accommodates + extpeop + min_stay + sentiment + secdep + cleanfee + weekfee + monthfee + bedroom + bathroom + beds, data = data2)
summary(modelm3)

modelm4 <- lm(logsavedwishes~ rating + price + accommodates + extpeop + min_stay + sentiment + secdep + cleanfee + weekfee + monthfee + bedroom + bathroom + beds, data = data2)
summary(modelm4)

modelm4_ft <- lm(logsavedwishes~logreviews + accommodates + min_stay + sentiment + secdep + cleanfee + monthfee + bathroom, data = data2)
summary(modelm4_ft)

modelm4_ft2 <- lm(logsavedwishes~logreviews + min_stay + sentiment + cleanfee + monthfee + bathroom, data = data2)
summary(modelm4_ft2)


# Correlation Matrix...Miami dataset

test <- select_if(data2, is.numeric)
test <- select(test, -c(logreviews, logsavedwishes))
crm <- cor(test, use = "complete.obs", method = "pearson")
ggcorrplot(crm, ggtheme = ggplot2::theme_minimal, show.diag = F, method = "circle", type="lower", lab=TRUE, lab_size=4) + labs (title = "Features correlation matrix") + theme(plot.title = element_text(hjust = 0.5))

hist(data2$logreviews)
hist(data2$logsavedwishes)