rm(list=ls())

library(readr)
library(dplyr)

#Read data and make data table
data <- read_delim("C:/Users/jfalter/Dropbox/Data Science/Titanic/train.csv", 
                            ",", escape_double = FALSE, trim_ws = TRUE, guess_max=100)

data<-data.table(data)

#Create impute function
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

#Imputing based on age
#data<-data[, Age := impute.mean(Age), by = Sex]

#Extracting Titles
data$Title <-str_extract(string = data$Name,pattern = "(Mr|Master|Mrs|Miss)\\.")

#Imputing based on Titles
data<-data[, Age := impute.mean(Age), by = Title]
