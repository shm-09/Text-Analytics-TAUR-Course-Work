#setwd(" set it to the directory of the source code and data")

############setting the working directory can also be automated by the following
############ but it causes some funny restarting of R that you need to live through

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#################

#install.packages("sentimentr")
#install.packages("tm")

library(tm)
library(sentimentr)
library(xlsx)

###################Read the text file###############
mdat<-read.xlsx("/Users/poojasengupta/Documents/Teaching/Text Analytics/Case/M-0897X1-week9 miami v2.xlsx", sheetName = "M-0897X1-week9")
#abrev<-read.csv("abmiamitext.csv")
str(mdat)
colnames(mdat)
abrev<-matrix(mdat[,19],ncol=1)
head(abrev)

##########Read Text Data###################################
sentdat<-NULL
for (i in 1:nrow(abrev)){
          
                          text<-abrev[i,]
        #text<-c("We had so much fun in South Beach! Merce's apartment is conveniently located close to the beach. Merce was very helpful in recommending us to a great restaurant nearby and another place within walking distance.")
                          require(tm)
                          arev<-Corpus(VectorSource(text))

###########preprocess Text Data####################
                          revc1<-tm_map(arev,removePunctuation)
                          revc1<-tm_map(revc1,removeNumbers)
                          revc1<-tm_map(revc1,tolower)
                          revc2<-tm_map(revc1,PlainTextDocument)

                          
################collect polarity scores for each property####################
                          sent=sentiment(revc2[[1]]$content)$sentiment
                          sentdat<-rbind(sentdat,sent)
                          cat("sentiment score for property #:",i,"=",sent,"\n")
                          
                          
}

write.csv(sentdat,file="/Users/poojasengupta/Documents/Teaching/Text Analytics/Case/sentimentscores_miami.csv")

data_miami<-read.xlsx("/Users/poojasengupta/Documents/Teaching/Text Analytics/Case/M-0897X1-week9 miami v2.xlsx", sheetName = "M-0897X1-week9")
str(data_miami)
hist(data_miami$reviews)
hist(data_miami$savwish)
hist(data_miami$logreviews)
hist(data_miami$logsavedwishes)


model_miami<-lm(logreviews~price+rating+accommodates+extpeop+min_stay+sentiment+secdep+
                  cleanfee+weekfee+monthfee+bedroom+bathroom+beds, data=data_miami)
summary(model_miami)

data_paris<-read.xlsx("/Users/poojasengupta/Documents/Teaching/Text Analytics/Case/M-0897X2 paris v2.xlsx", sheetName = "M-0897X2 paris v2")
str(data_paris)
hist(data_paris$reviews)
hist(data_paris$savwish)
hist(data_paris$logreviews)
hist(data_paris$logsavwish)


model_paris<-lm(reviews~price+rating+accommodates+extpeop+min_stay+sentiment+secdep+
                  cleanfee+weekfee+monthfee+bedroom+bathroom+beds, data=model_paris)
summary(model_paris)







