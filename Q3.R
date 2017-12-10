library(caret)

1. 
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

#Set the variable y to be a factor variable in both the training and test set. 
#Then set the seed to 33833. Fit (1) a random forest predictor relating the factor 
#variable y to the remaining variables and (2) a boosted predictor using the "gbm" method. 
#Fit these both with the train() command in the caret package.

vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)
set.seed(33833)

mod1 <- train(y ~ ., method = "rf", data = vowel.train)
mod2 <- train(y ~., method="gbm", data = vowel.train)

pred1 <- predict(mod1, vowel.test)
pred2 <- predict(mod2, vowel.test)

a1 <- sum( pred1 == vowel.test$y) / length(pred1)
a2 <- sum( pred2 == vowel.test$y) / length(pred2)

sum( pred1 == vowel.test$y & pred1 == pred2) / sum (pred1 == pred2)

#What are the accuracies for the two approaches on the test data set? 
#What is the accuracy among the test set samples where the two methods agree?

Try:
  #RF Accuracy = 0.6082, GBM Accuracy = 0.5152, Agreement Accuracy = 0.6361
  

2.
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

#Set the seed to 62433 and predict diagnosis with all the other variables using a random forest ("rf"), 
#boosted trees ("gbm") and linear discriminant analysis ("lda") model. 
#Stack the predictions together using random forests ("rf").

set.seed(62433)

mod1 <- train(diagnosis ~ ., method = "rf", data = training)
mod2 <- train(diagnosis ~ ., method = "gbm", data = training)
mod3 <- train(diagnosis ~ ., method = "lda", data = training)

pred1 <- predict(mod1, testing)
pred2 <- predict(mod2, testing)
pred3 <- predict(mod3, testing)

predDF <- data.frame (pred1, pred2, pred3, diagnosis = testing$diagnosis)

modC <- train(diagnosis ~ ., method = "rf", data = predDF)

pred <- predict(modC, testing)

sum( pred1 == testing$diagnosis) / dim(testing)[1]
sum( pred2 == testing$diagnosis) / dim(testing)[1]
sum( pred3 == testing$diagnosis) / dim(testing)[1]
sum( pred == testing$diagnosis) / dim(testing)[1]


#What is the resulting accuracy on the test set? 
#Is it better or worse than each of the individual predictions?

Try:
  Stacked Accuracy: 0.80 is better than all three other methods
  Stacked Accuracy: 0.88 is better than all three other methods


3.
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

#Set the seed to 233 and fit a lasso model to predict Compressive Strength. 
#Which variable is the last coefficient to be set to zero as the penalty increases? 
#(Hint: it may be useful to look up ?plot.enet).

set.seed(233)
mod <- train(CompressiveStrength ~ ., method = "lasso", data = training)
pred <- predict(mod, testing)

object <- enet(x=testing$Cement, y=pred, lambda = 0)

plot(mod)

try:
  Cement


4. 

library(lubridate) # For year() function below
setwd("E:/My Documents/R/C8")
dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

#Fit a model using the bats() function in the forecast package to the training time series. 
#Then forecast this model for the remaining time points. 
#For how many of the testing points is the true value within the 95% prediction 
#interval bounds? 

library(forecast)

fit <- bats(training$visitsTumblr)
pred <- forecast(fit, data=testing)

(dim(testing)[1] - length(pred)) / dim(testing)[1]

Try:
  96%

5.
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

#Set the seed to 325 and fit a support vector machine using the e1071 package 
#to predict Compressive Strength using the default settings. 
#Predict on the testing set. What is the RMSE?

set.seed(325)

library(e1071)
library(ModelMetrics)

mod <- svm(CompressiveStrength ~ ., training)

pred <- predict(mod, testing)

rmse (testing$CompressiveStrength, pred)

Try:
  6.72
