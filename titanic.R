rm(list=ls())

library(readr)
library(dplyr)
library(data.table)
library(stringr)
#Read data and make data table
data <- read_delim("C:/Users/jfalter/Dropbox/Data Science/GitHub/Titanic/train.csv", 
                            ",", escape_double = FALSE, trim_ws = TRUE, guess_max=100)

#not sure which to use
data<-data.table(data)
data<-data.frame(data)

#Create impute function
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

#Imputing based on age
#data<-data[, Age := impute.mean(Age), by = Sex]

#Extracting Titles
data$Title <-str_extract(string = data$Name,pattern = "(Mr|Master|Mrs|Miss)\\.")

#Ticket numbers
data$TicketNumber<-str_extract(string = data$Ticket,pattern = "^[0-9]+$")

#Ticket Prefixes (not quite right)
data$TicketPrefix<-str_extract(string = data$Ticket,pattern = "[^0-9]{1,8}")

data$TicketPrefix<-NULL

#Imputing based on Titles
data<-data[, Age := impute.mean(Age), by = Title]

#Creating family variable
data$FamilySize<-data$SibSp+data$Parch

#Throwing away variables we're not going to use
data<-data[,c(2,3,5,6,7,8,10,12,13)]

#Replace NA Embarked with Southampton
which(is.na(data$Embarked))
data$Embarked[c(62,830)] = 'S'

#Convert from character vectors to factors
data$Embarked<-as.factor(data$Embarked)
data$Survived<-as.factor(data$Survived)
data$Sex<-as.factor(data$Sex)

library(randomForest)

set.seed(0)

fit <-randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + FamilySize + Title,
                   data=data,
                   importance=TRUE,
                   ntree=2000)

#Figuring out which Titles are NAs
which(is.na(data$Title))

#What's the best way to impute the missing Titles? By age and sex?
