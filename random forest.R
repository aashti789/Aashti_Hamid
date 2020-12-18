credit <- read.csv('C:/R dataset/credit_data.csv')
View(credit)
str(credit)
#Change the dependent variable into factor.
credit$Creditability <- sapply(credit$Creditability, factor)
str(credit)
dim(credit)
#Divinding the data into training and test datasets.
library(caTools)
library(dplyr)
set.seed(1)
s <- sample.split(credit$Creditability, SplitRatio = 0.7)
credit_train <-  filter(credit, s== TRUE)
dim(credit_train)
credit_test <- filter(credit, s== FALSE)
dim(credit_test)
#Building the random forest model:
library(randomForest)
credit_model <- randomForest(Creditability~., data= credit_train, mtry= 8, ntree= 200)
pred_cred <- predict(credit_model, credit_test)
library(caret)
confmat <- confusionMatrix(pred_cred, credit_test$Creditability)
confmat
##Accuracy is 75%
#Let us try to increase the accuracy of the model created.

#Tuning the random forest model.
mtry <-  tuneRF(credit_train[-1], credit_train$Creditability, ntreeTry = 500, stepFactor = 1.5, improve = 0.02, trace= TRUE, plot = TRUE)

#Again building the random forest model:
credit_model1 <- randomForest(Creditability~., data = credit_train, ntry= 500, mtry= 6)
pred_cred1 <-  predict(credit_model1, credit_test, type= 'response')
confmat1 <- confusionMatrix(pred_cred1, credit_test$Creditability)
confmat1
#Accuracy dosen't increase much.