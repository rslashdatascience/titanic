rm(list=ls())

library(readr)
library(dplyr)
library(data.table)
library(stringr)

#Read in training and test data and combine them
train <- read_delim("C:/Users/jfalter/Dropbox/Data Science/GitHub/Titanic/titanic/train.csv", 
                            ",", escape_double = FALSE, trim_ws = TRUE, guess_max=100)

test <- read_delim("C:/Users/jfalter/Dropbox/Data Science/GitHub/Titanic/titanic/test.csv", 
                   ",", escape_double = FALSE, trim_ws = TRUE, guess_max=100)

#Add Survived column and rearrange data.frame
test$Survived<-NA
test<-test[,c(1,12,2,3,4,5,6,7,8,9,10,11)]

#Adding tags for training and test set to each df
train$Mask<-'Train'
test$Mask<-'Test'

#Add dfs together
combined<-rbind(test, train)
combined<-data.table(combined)

#Create impute function
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

#Imputing based on age
#train<-train[, Age := impute.mean(Age), by = Sex]

#Extracting Titles
combined$Title <-str_extract(string = combined$Name,pattern = "(Mr|Master|Mrs|Miss)\\.")

#Ticket numbers
combined$TicketNumber<-str_extract(string = combined$Ticket,pattern = "^[0-9]+$")

#Ticket Prefixes (not quite right)
combined$TicketPrefix<-str_extract(string = combined$Ticket,pattern = "[^0-9]{1,8}")

combined$TicketPrefix<-NULL

#Imputing based on Titles
combined<-combined[, Age := impute.mean(Age), by = Title]

#Creating family variable
combined$FamilySize<-combined$SibSp+combined$Parch

#Throwing away variables we're not going to use
combined<-combined[,c(2,3,5,6,7,8,10,12,13,14,16)]

#Convert from character vectors to factors
combined$Embarked<-as.factor(combined$Embarked)
combined$Survived<-as.factor(combined$Survived)
combined$Sex<-as.factor(combined$Sex)
combined$Title<-as.factor(combined$Title)
summary(combined)

#Replace NA Embarked with Southampton (which is the mode)]
which(is.na(combined$Embarked))
combined$Embarked[c(480,1248)] = 'S'


#Split out into training and test sets
combined<-data.frame(combined)
test_imputed<-combined[combined$Mask=='Test',c(1:8,10,11)]
train_imputed<-combined[combined$Mask=='Train',c(1:8,10,11)]

#Looking for NAs
summary(train_imputed)
#They are in the Title variable

#The next few sections are not completed yet.

counts=xtabs(~Survived + Title, train_imputed)
barplot(counts, main="Survival by Title", xlab="Title")

#Two ideas: 1. Find the average age of each Title, then impute NAs based on those and Gender. 2. Keep NA as a separate
#group

#Check this
counts2<-table(train_imputed$Survived, train_imputed$Title)

barplot(counts2, legend=rownames(counts), beside=TRUE)


library(randomForest)

set.seed(0)

fit <-randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + FamilySize + Title,
                   data=train_imputed,
                   importance=TRUE,
                   ntree=2000)

#Figuring out which Titles are NAs
which(is.na(train$Title))


#Creating a correlation matrix
cor_train<-train[,c(1,2,4,5,6,7)]
cor_train<-as.train.frame(cor_train)
cor(cor_train[sapply(cor_train, is.numeric)])
