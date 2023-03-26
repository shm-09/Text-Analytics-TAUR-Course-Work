remove(list=ls())

# first row contains variable names, comma is separator
# assign the variable id to row names
# note the / instead of \ on mswindows systems

# read in the first worksheet from the workbook myexcel.xlsx
# first row contains variable names
library(readxl)
mydata <- read_xlsx("C:/Users/Shreyansh Mohanty/Desktop/IIM Ranchi MBA-BA Files/Term-6/TAUR/R Programs/Datasets/Cereal Data.xlsx", 1)

# read in the worksheet named mysheet
mydata2 <- read_xlsx("C:/Users/Shreyansh Mohanty/Desktop/IIM Ranchi MBA-BA Files/Term-6/TAUR/R Programs/Datasets/Cereal Data.xlsx", sheet = "Cereal Data")

# read the csv 
mydatacsv <- read_csv("C:/Users/Shreyansh Mohanty/Desktop/IIM Ranchi MBA-BA Files/Term-6/TAUR/R Programs/Datasets/Future50_restaurants.csv", col_names = TRUE)
#library(tidyverse)
mydatacsv2 <- read_csv("C:/Users/Shreyansh Mohanty/Desktop/IIM Ranchi MBA-BA Files/Term-6/TAUR/R Programs/Datasets/Future50_restaurants.csv", col_names = FALSE)
head(mydata)
#####################Subsetting data######################

library(tidyverse)

d <- read_csv("https://stats.idre.ucla.edu/stat/data/rdm/patient_pt1_dm.csv")
str(d)
View(d)
head(d)
# d is of class tibble (tbl_df), class table (tbl) and class data.frame
class(d)

# now just a data.frame
class(as.data.frame(d))

######Subsetting and extracting data from a data frame#################

# row 3 column 2
d[3,2]

# rows 2 and 4 of columns age
d[c(2,4), "age"]

# rows 2,3 & 4 of columns age
d[c(2:4), "age"]

# all columns for row 10
d[10,]

# all rows of column "pain"
d[,"pain"]

# subsetting with $ creates a numeric vector, which itself can be subset
d$age[2:3]

# logical comparison results in a logical vector
d$age > 60

# when placed in [] before the comma, rows with TRUE are selected
#  this returns columns age and pain for observations where age > 60
d[d$age>60, c("age", "pain")]

# specifying arguments by name
seq(from=1, to=5, by=1)

# specifying arguments by position
seq(10, 0, -2)

summary(d$sex)

library(Hmisc)

# detailed summaries of variables
describe(d)

describe(d[,c("age", "sex", "test1")])

#plot(describe(d))

# change all impossible age values to NA 
#  assume all adults in this dataset
d$age[d$age<18|d$age>120] <- NA

# remember to use quotes for character variables
d$sex[d$sex == "12.2"] <- NA

# Setting missing data or default or wrong data as NA...
d[d==-99] <- NA
d[d==-98] <- NA
d[d=="not assessed"] <- NA

# which cases are complete
complete.cases(d)

# number of complete cases
sum(complete.cases(d))

# create a dataset of complete cases
d_comp <- d[complete.cases(d),]

#plot(describe(d))

#plot(describe(d), which="continuous")

# all variables for patients with pain>=9
pain9 <- d[d$pain>=9,]
pain9

# subset to females with pain >= 9
dfhp <- filter(d, sex=="female", pain>=9)
dfhp

# select those not in UCLA, who are either younger than 40 or older than 60
temp1 <- filter(d, hospital!="UCLA", age<40 | age>60)
temp1

temp2 <- filter(d, hospital!="UCSF", age<20 | age>40)
temp2

# select 4 variables
d_small <- select(d, hospid, docid, age, cancerstage)
head(d_small, n=3)
names(d_small)  # names of the coloumns extracted from the dataframe...

# get hospital variables
dhosp <- select(d, starts_with("hosp"))
names(dhosp)

# get "test1" and "test2" variables using num_range
tests <- select(d, num_range("test", 1:2))
names(tests)

# put the two test vars first while renaming them, 
#   then everything else 
tests_first <- select(d, t1=test1, t2=test2, everything())
names(tests_first)

# sort, by age, oldest first
arrange(d, desc(age)) 

# sort, by age, young first
arrange(d, age) 

# create age category variable, and highpain binary variable
d <- mutate(d,
            agecat = cut(age, breaks=c(30,40,50,60,70,120)),
            highpain = pain > mean(pain))
table(d$agecat, d$highpain) #creates a contingency table

# group_by creates a grouped_df (grouped data frame) class structure
by_doc <- group_by(d, docid)
class(by_doc)

# Create summaries of patients by doctor
pat_summ <- summarise(by_doc,
                      n_pat = n(), # number of patients
                      longest_los = max(lengthofstay), # longest length of stay
                      avg_age = mean(age), # average age
                      n_stage_four = sum(cancerstage=="IV") # number of stage IV patients 
)
pat_summ

# a dataset of age and pain for females younger than 40
f40 <- filter(d, sex=="female" & age<40)
f40_small <- select(f40, age, pain)
f40_small

##################Reshaping Your Data with tidyr#########################

library(tidyverse)
seps <- read_csv("http://www.mm-c.me/mdsi/hospitals93to98.csv")
head(seps)
str(seps)

inprogress<-gather(seps,year,value,FY1993:FY1998)
head(inprogress)

rearranged <- spread(inprogress,Field,value)
head(rearranged)

seps %>%
  gather(year,value,FY1993:FY1998) %>%
  spread(Field,value)


##########Pipe functions###########

# Initialize `x`
x <- c(0.109, 0.359, 0.63, 0.996, 0.515, 0.142, 0.017, 0.829, 0.907)

# Compute the logarithm of `x`, return suitably lagged and iterated differences, 
# compute the exponential function and round the result
round(exp(diff(log(x))), 1)

# Import `magrittr`
library(magrittr)

# Perform the same computations on `x` as above
x %>% log() %>%
  diff() %>%
  exp() %>%
  round(1)

# Initialize `x` 
x <- rnorm(100)

# Update value of `x` and assign it to `x`
x %<>% abs %>% sort

# Compute the logarithm of `x` 
log(x)

# Compute the logarithm of `x` 
x %>% log()

#This means that functions that take one argument, function(argument), can be rewritten 
#as follows: argument %>% function().

# Round pi
round(pi, 6)

# Round pi 
pi %>% round(6)

# Import `babynames` data
library(babynames)
# Import `dplyr` library
library(dplyr)

# Load the data
data(babynames)

# Count how many young boys with the name "Taylor" are born
sum(select(filter(babynames,sex=="M",name=="Taylor"),n))

# Do the same but now with `%>%`
babynames%>%filter(sex=="M",name=="Taylor")%>%
  select(n)%>%
  sum

library(hflights)

grouped_flights <- group_by(hflights, Year, Month, DayofMonth)
flights_data <- select(grouped_flights, Year:DayofMonth, ArrDelay, DepDelay)
summarized_flights <- summarise(flights_data, 
                                arr = mean(ArrDelay, na.rm = TRUE), 
                                dep = mean(DepDelay, na.rm = TRUE))
final_result <- filter(summarized_flights, arr > 30 | dep > 30)

final_result
view(flights_data)
view(summarized_flights)
view(final_result)

res_temp <- hflights %>% 
  group_by(Year, Month, DayofMonth) %>% 
  select(Year:DayofMonth, ArrDelay, DepDelay) %>% 
  summarise(arr = mean(ArrDelay, na.rm = TRUE), dep = mean(DepDelay, na.rm = TRUE)) %>% 
  filter(arr > 30 | dep > 30)

view(res_temp)

###############################Mutate#####################

library("dplyr") 

hflights %>%
  mutate(AirTime_hrs = AirTime / 60)

hflights %>%
  mutate(AirTime_hrs = AirTime / 60) %>%
  head

hflights %>%
  mutate(AirTime_hrs = AirTime / 60) %>%
  filter(!is.na(AirTime)) %>%
  head


####################################Some uses and examples######################


# start with d, then filter rows, then select variables
f40_small <- d %>%   
  filter(sex=="female" & age<40) %>% 
  select(age, pain)
f40_small

# create a plot of average age vs tumor size 
#   by doctors, for doctors with more than 5 patients
# g1 <- d %>%     
#   group_by(docid) %>%  
#   summarise(n_pat=n(),   
#             avg_age=mean(age), 
#             avg_tumor=mean(tumorsize)) %>%   
#   filter(n_pat > 5) %>%  
#   ggplot(aes(x=avg_age, y=avg_tumor)) + geom_point() +
#   geom_smooth() 

# g1

d %>% 
  filter(age < 40) %>% 
  lm(tumorsize ~ age, data=.) # the . is the filtered dataset

################################################################################
x <- c(1:4, NA, 6:7, NA)
x
x[is.na(x)] <- mean(x, na.rm = TRUE) #replace the missing values of x with the mean

########################Graphics using ggplot2 package##########################

library(ggplot2)
dat <- ggplot2::mpg

dat <- transform(dat,
                 cyl = factor(cyl),
                 drv = factor(drv),
                 fl = factor(fl),
                 year = factor(year),
                 class = factor(class)
)

######Scatterplot#######

ggplot(dat) + # data
  aes(x = displ, y = hwy) + # variables
  geom_point() # type of plot

ggplot(dat, aes(x = displ, y = hwy)) +
  geom_point()

######Line plots########

ggplot(dat) +
  aes(x = displ, y = hwy) +
  geom_line()

######Line and point plots########

ggplot(dat) +
  aes(x = displ, y = hwy) +
  geom_point() +
  geom_line() # add line

##########Histogram##########

ggplot(dat) +
  aes(x = hwy) +
  geom_histogram()


######Default bin is 30, can change it using following code #########

ggplot(dat) +
  aes(x = hwy) +
  geom_histogram(bins = sqrt(nrow(dat)))

######################Density plots################

ggplot(dat) +
  aes(x = hwy) +
  geom_density()

#########Histogram and density####################

ggplot(dat) +
  aes(x = hwy, y = ..density..) +
  geom_histogram() +
  geom_density()

# Boxplot for one variable
ggplot(dat) +
  aes(x = "", y = hwy) +
  geom_boxplot()

# Boxplot by factor
ggplot(dat) +
  aes(x = drv, y = hwy) +
  geom_boxplot()

#####Bar plot ##############

ggplot(dat) +
  aes(x = drv) +
  geom_bar()

ggplot(dat) +
  aes(x = drv, fill = drv) + # add colors to bars
  geom_bar() +
  theme(legend.position = "none") # remove legend

########a barplot with two qualitative variables##########

ggplot(dat) +
  aes(x = drv, fill = year) + # fill by years
  geom_bar()

####In order to compare proportions across groups, it is best to make each bar the same height

ggplot(dat) +
  geom_bar(aes(x = drv, fill = year), position = "fill")

###################Further personalisation of ggplots#####################

#####Title and axis labels#####

p <- ggplot(dat) +
  aes(x = displ, y = hwy) +
  geom_point()
p + labs(
  title = "Fuel efficiency for 38 popular models of car",
  subtitle = "Period 1999-2008",
  caption = "Data: ggplot2::mpg. See more at www.statsandr.com",
  x = "Engine displacement (litres)",
  y = "Highway miles per gallon (mpg)"
)

#####the size and the shape of the title and subtitle#######

p + labs(
  title = "Fuel efficiency for 38 popular models of car",
  subtitle = "Period 1999-2008",
  caption = "Data: ggplot2::mpg. See more at www.statsandr.com",
  x = "Engine displacement (litres)",
  y = "Highway miles per gallon (mpg)"
) +
  theme(
    plot.title = element_text(
      hjust = 0.5, # center
      size = 12,
      color = "steelblue",
      face = "bold"
    ),
    plot.subtitle = element_text(
      hjust = 0.5, # center
      size = 10,
      color = "gray",
      face = "italic"
    )
  )


#################If the title or subtitle is long and you want to divide it into multiple lines#######

p + labs(
  title = "Fuel efficiency for 38 popular \n models of car",
  subtitle = "Period 1999-2008",
  caption = "Data: ggplot2::mpg. See more at www.statsandr.com",
  x = "Engine displacement (litres)",
  y = "Highway miles per gallon (mpg)"
) +
  theme(
    plot.title = element_text(
      hjust = 0.5, # center
      size = 12,
      color = "steelblue",
      face = "bold"
    ),
    plot.subtitle = element_text(
      hjust = 0.5, # center
      size = 10,
      color = "gray",
      face = "italic"
    )
  )

##########################Axis ticks###############################

# Adjust ticks
p + scale_x_continuous(breaks = seq(from = 1, to = 7, by = 0.5)) + # x-axis
  scale_y_continuous(breaks = seq(from = 10, to = 45, by = 5)) # y-axis

##################Control axis limits#################

p + scale_x_continuous(limits = c(3, 6)) +
  scale_y_continuous(limits = c(20, 30))
















