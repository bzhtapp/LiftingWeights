library(caret)

setwd("E:/My Documents/R/C8")

http://groupware.les.inf.puc-rio.br/har


#The goal of your project is to predict the manner in which they did the exercise. 
#This is the "classe" variable in the training set. 
#You may use any of the other variables to predict with. 
#You should create a report describing 
# 1. how you built your model, 
# 2. how you used cross validation, what you think the expected out of sample error is, 
# 3. and why you made the choices you did. 
#You will also use your prediction model to predict 20 different test cases. 

set.set(100)
training = read.csv("pml-training.csv")
testing = read.csv("pml-testing.csv")

summary(training$classe)

mod <- train(classe ~ ., method = "rf", data = training, na.action=na.exclude)

dim(training)
head(training$classe)
