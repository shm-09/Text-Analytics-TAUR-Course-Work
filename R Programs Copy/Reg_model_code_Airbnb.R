rm(list = ls())

data_miami<-read.xlsx("/Users/poojasengupta/Documents/Teaching/Text Analytics/Case/M-0897X1-week9 miami v2.xlsx", sheetName = "M-0897X1-week9")
str(data_miami)
hist(data_miami$reviews)
hist(data_miami$savwish)
hist(data_miami$logreviews)
hist(data_miami$logsavedwishes)


model_miami<-lm(logreviews~price+rating+accommodates+extpeop+min_stay+sentiment+secdep+
                  cleanfee+weekfee+monthfee+bedroom+bathroom+beds, data=data_miami)
summary(model_miami)

model_miami2<-lm(logsavedwishes~price+rating+accommodates+extpeop+min_stay+sentiment+secdep+
                   cleanfee+weekfee+monthfee+bedroom+bathroom+beds, data=data_miami)
summary(model_miami2)

data_paris<-read.xlsx("/Users/poojasengupta/Documents/Teaching/Text Analytics/Case/M-0897X2 paris v2.xlsx", sheetName = "M-0897X2 paris v2")
str(data_paris)
hist(data_paris$reviews)
hist(data_paris$savwish)
hist(data_paris$logreviews)
hist(data_paris$logsavwish)


model_paris<-lm(logreviews~price+rating+accommodates+extpeop+min_stay+sentiment+secdep+
                  cleanfee+weekfee+monthfee+bedroom+bathroom+beds, data=data_paris)
summary(model_paris)

model_paris<-lm(logsavwish~price+rating+accommodates+extpeop+min_stay+sentiment+secdep+
                  cleanfee+weekfee+monthfee+bedroom+bathroom+beds, data=data_paris)
summary(model_paris)





