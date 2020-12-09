svm <- read.csv('C:/R dataset/Demo 1_ Support Vector Machines.csv')
View(svm)
str(svm)
count(svm$Churn)
#Changing Churn variable from integer to factor, using sapply.
library(dplyr)
svm$Churn <- sapply(svm$Churn, factor)
str(svm)
View(svm)

#Dividing the data into training and test datasets.
library(caTools)
set.seed(100)
s <- sample.split(svm$Churn, SplitRatio = 0.7)
svm_train <-  filter(svm, s== TRUE)
dim(svm_train)
svm_test <- filter(svm, s== FALSE)
dim(svm_test)

#Building the SVM model
library(e1071)
svm_model <- svm(Churn~., data= svm_train)
summary(svm_model)
prediction_train <-  predict(svm_model, svm_train)
prediction_train
#Analysing the model made:
library(caret)
ConMat_train <- confusionMatrix(table(prediction_train, svm_train$Churn), positive = '1')
ConMat_train
#SVM Model made for the training data is 92% accurate.

prediction_test <- predict(svm_model, svm_test)
prediction_test
ConMat_Test <- confusionMatrix(table(prediction_test, svm_test$Churn), positive = '1')
ConMat_Test
#So the accuracy of the model for the test data is 89%.