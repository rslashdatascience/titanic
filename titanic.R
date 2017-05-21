rm(list=ls())

library(readr)
library(dplyr)
library(data.table)
library(stringr)

#STEP 1
#Read in training and test data
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

#Getting levels of the ship from Cabin
combined$Level<-str_extract(string=combined$Cabin, pattern = "[A-Z]")
combined<-data.table(combined)
#Imputing based on Titles
combined<-combined[, Age := impute.mean(Age), by = Title]
combined<-data.frame(combined)
#Creating family variable
combined$FamilySize<-combined$SibSp+combined$Parch

#Throwing away variables we're not going to use
combined<-combined[,c(1,2,3,5,6,7,8,10,12,13,14,16,17)]

#Convert from character vectors to factors
combined$Embarked<-as.factor(combined$Embarked)
combined$Survived<-as.factor(combined$Survived)
combined$Sex<-as.factor(combined$Sex)
combined$Title<-as.factor(combined$Title)
combined$Level<-as.factor(combined$Level)
summary(combined)

#Data imported and cleaned up now
#STEP 2: Data Munging

#Deal with Missing Data, NAs.
summary(combined) #NAs in Survived, Age, Embarked...

summary(combined$Embarked) #Southampton is the mode
#Replace NA Embarked with Southampton
which(is.na(combined$Embarked))
combined$Embarked[c(480,1248)] = 'S'

summary(combined$Title)
#LThey are in the Title variable
missing_titles<-which(is.na(combined$Title))

#Convert Title from factor to character #Alternatively, I can impute Title based on age
combined$Title<-as.character(combined$Title)
combined$Title[missing_titles]='Missing'


#Also in fare
combined$Fare[is.na(combined$Fare)]=mean(combined$Fare, na.rm=TRUE)
#Split out into training and test sets
combined<-data.frame(combined)
#Convert back to factor
combined$Title<-as.factor(combined$Title)

#And in Level
combined$Level<-as.character(combined$Level)
missing_levels<-which(is.na(combined$Level))
combined$Level[missing_levels]='Missing'
combined$Level<-as.factor(combined$Level)

test_imputed<-combined[combined$Mask=='Test',c(1:9,11,12,13)]
train_imputed<-combined[combined$Mask=='Train',c(1:9,11,12,13)]


#STEP 3: Visualizations
#Checkitout
counts<-table(train_imputed$Survived, train_imputed$Title)
barplot(counts, legend=rownames(counts), beside=TRUE, main="Survival by Title")

counts<-table(train_imputed$Survived, train_imputed$FamilySize)
barplot(counts, legend=rownames(counts), beside=TRUE, main="Survival by Family Size")

#STEP 4: Model Building
#Random Forest
library(randomForest)

set.seed(0)

fit <-randomForest(Survived ~ Pclass + Sex + Age  +  Embarked + Title + Level + Fare + FamilySize,
                   data=train_imputed,
                   importance=TRUE,
                   ntree=5000)

#Now predicting
prediction <- predict(fit, test_imputed)
submission<-data.frame(PassengerID=test_imputed$PassengerId, Survived=prediction)
write.csv(submission, file="thirdforestinR.csv", row.names=FALSE)

#Logistic Regression?
fit<-glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + FamilySize + Title + Level,
         data=train_imputed,
         family=binomial(link='logit'))

summary(fit) #Looks like Pclass, Age, SibSp, Parch, EmbarkedS, TitleMissing and Title Mr are very predictive

missing_levels<-which((combined$Level) != 'Missing')
missing_levels<-which(is.na(combined$Level))
combined$Level<-as.character(combined$Level)
combined$Level[missing_levels]='Have'
combined$Level<-as.factor(combined$Level)

fit<-glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Embarked + Title + Level,
         data=train_imputed,
         family=binomial(link='logit'))

#Pretty much the same as above but removing FamilySize and lets condense Level to 'Have' vs 'Missing'
summary(fit)



#Now predicting
prediction <- predict(fit, test_imputed)
submission<-data.frame(PassengerID=test_imputed$PassengerId, Survived=prediction)
write.csv(submission, file="thirdlogitinR.csv", row.names=FALSE)

#STill resulting in about 80% accuracy.  Next thing to try: Extract family names and create a variable for each family
#with >2 people

#SVM?
library(e1071)

fit<-svm(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title,
         data=train_imputed)
x=train_imputed[,c(1,3:11)]
y=train_imputed[,2]

#Can't get the following code to work.
svm_tune <- tune(svm, train.x=train_imputed[,c(1,3:11)], train.y=train_imputed[,2], data=train_imputed,
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
#NVM got it:
svm_tune <- tune(svm, Survived~., data = train_imputed, 
            ranges = list(gamma = 2^(-1:1), cost = 2^(2:4)),
            tunecontrol = tune.control(sampling = "fix"))
print(svm_tune) # Gamma 1, Cost 4

fit<-svm(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + FamilySize + Title,
         data=train_imputed, gamma=1, cost =4)


#Now predicting
#Changing test_imputed's NAs to 0s so the prediction will run
test_imputed$Survived[which(is.na(test_imputed$Survived))]=0
prediction <- predict(fit, test_imputed)
submission<-data.frame(PassengerID=test_imputed$PassengerId, Survived=prediction)
write.csv(submission, file="thirdSVMinR.csv", row.names=FALSE)

prediction<-predict(fit, train_imputed)
table(prediction, train_imputed$Survived)

#Standard Deviatations of Age, SibSp, Parch and Fare
stdevs<-sapply(combined[,5:8], sd)  #Age=13.19, SibSp=1.04, Parch=0.86, Fare=


#Creating a correlation matrix
cor_train<-train[,c(1,2,4,5,6,7)]
cor_train<-as.train.frame(cor_train)
cor(cor_train[sapply(cor_train, is.numeric)])

#Looks like I might need to make some more variables

#NaiveBayes?
fit<-naiveBayes(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + FamilySize + Title + Level,
                data=train_imputed)
summary(fit)
prediction <- predict(fit, test_imputed)
submission<-data.frame(PassengerID=test_imputed$PassengerId, Survived=prediction)
write.csv(submission, file="firstNBinR.csv", row.names=FALSE)
