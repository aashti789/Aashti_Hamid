bank_loan
View( bank_loan)
str(bank_loan)
install.packages('rpart')
install.packages('rpart.plot')
library(rpart)
library(rpart.plot)
library(caTools)
library(caret)
library(dplyr)

set.seed(100)
s <- sample.split(bank_loan$Default, SplitRatio = 0.8)
bank_loan_train <- filter(bank_loan, s== TRUE)
bank_loan_test <- filter(bank_loan, s== FALSE)

#Building decision tree model
dtree <- rpart(Default~., data= bank_loan, method = 'class')
dtree
summary(dtree)
prp(dtree)
predict_test <- predict(dtree, bank_loan_test, type = 'class')
predict_test
confusionMatrix(predict_test, bank_loan_test$Default)

#K-fold cross  validation
folded_up <- createFolds(bank_loan, k=10, list = TRUE, returnTrain = FALSE)
train_set <- names(folded_up[1])
bank_loan[folded_up$train_set,]
