1. 
library(AppliedPredictiveModeling)
data(AlzheimerDisease)


#Which of the following commands will create non-overlapping training and test sets with about 
#50% of the observations assigned to each?

Answer: 
  One with alternating signs


2.
library(Hmisc)
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]


#Make a plot of the outcome (CompressiveStrength) versus the index of the samples. 
#Color by each of the variables in the data set (you may find the cut2() 
#function in the Hmisc package useful for turning continuous covariates into factors).
#What do you notice in these plots?

?cut2
head(training,30)
tail(training,30)


plot (y=training$CompressiveStrength, colour=)

Answer: 
  There is a non-random pattern in the plot of the outcome versus index that 
  does not appear to be perfectly explained by any predictor suggesting a variable may be missing.


3.
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

#Make a histogram and confirm the SuperPlasticizer variable is skewed. Normally you might use the log 
#transform to try to make the data more symmetric. Why would that be a poor choice for this variable?

summary(training)

hist(log(training$Superplasticizer))

Tried:
  The log transform is not a monotone transformation of the data.

4.
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[inTrain,]
testing = adData[-inTrain,]


#Find all the predictor variables in the training set that begin with IL. 
#Perform principal components on these variables with the preProcess() function from the caret package. 
#Calculate the number of principal components needed to capture 80% of the variance. 
#How many are there?

head(training)
summary(training)
names(training)

df <- training[,grep("^IL", names(training))]

dim(df)
head(df)

?
pc <- preProcess(df, method="pca")

head(pc)

df.pca <- prcomp(df,
                 center = TRUE,
                 scale. = TRUE) 

summary(df.pca)

Answer:
  7

5.
library(caret)
library(AppliedPredictiveModeling)
library(e1071)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

Create a training data set consisting of only the predictors with variable names beginning 
with IL and the diagnosis. Build two predictive models, one using the predictors as 
they are and one using PCA with principal components explaining 80% of the variance in the predictors. 
Use method="glm" in the train function. 
What is the accuracy of each method in the test set? Which is more accurate?

head(training)
df <- training[,grep("^IL", names(training))]
df$diagnosis <- training[, "diagnosis"]

fit1 <- train (diagnosis ~ ., data=df, method="glm")
predict1 <- predict(fit1, testing)
accuracy1 <- sum(predict1 == testing$diagnosis)/dim(testing)[1]
accuracy1


fit2 <- train(diagnosis ~ ., data=df, method = "glm", preProcess = "pca",
             trControl = trainControl(preProcOptions = list(thresh=.8)))
predict2     <- predict(fit2, testing)
accuracy2 <- sum(predict2 == testing$diagnosis)/dim(testing)[1]
accuracy2


Answer:
Non PCA .65
PCA .72

