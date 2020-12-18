bank_loan <- read.csv('C:/R dataset/Demo 2_ Naive Bayes Classifier.csv')
View(bank_loan)
str(bank_loan)
#Changing Default from int to factor: Only to be done for classification
library('dplyr')
bank_loan$Default <- sapply(bank_loan$Default, factor)
str(bank_loan)
#Dividing the data into training and test datasets.
library(caTools)
s <- sample.split(bank_loan$Default, SplitRatio = 0.7)
bank_loan_train <- filter(bank_loan, s== TRUE)
bank_loan_test <-  filter(bank_loan, s== FALSE)
#Building the Naive Bayes Model.
library(e1071)
naive_model <- naiveBayes(Default~., data= bank_loan)
summary(naive_model)
naive_model

predict_train <- predict(naive_model, bank_loan_train)
predict_train
library(caret)
conf <- confusionMatrix(predict_train, bank_loan_train$Default)
conf
#76%accuracy is shown by the model with the training data.

predict_test <- predict(naive_model, bank_loan_test)
conf_test <- confusionMatrix(predict_test, bank_loan_test$Default)
conf_test
#75.6% accuracy is given by the test data.