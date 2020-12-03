library(readxl)
library(ggplot2)
library(dplyr)
SLR <- read.csv('C:/R dataset/Demo 1_Perform simple linear regression.csv')
View(SLR)
results <- lm(formula = SLR$Weight~SLR$Height)#weight is y and height is x
results
#to know the accuracy of the model
summary(results)
ggplot(data = SLR, mapping = aes(x=Height, y= Weight)) +geom_point()
#always p<alpha for the coeff to be accepted, since here p<alpha=> height is significant
#weight= -143.0269+3.899*height
##---------------------------------------------
housing <- read.csv('C:/R dataset/Housing_data.csv')
View(housing)
#Price=y, living area=x
results1 <- lm(formula = housing$Price~ housing$LivingArea)
results1
#Testing the accuracy of the model
summary(results1)
#Living Area is very significant as p<<<alpha
ggplot(data= housing, mapping = aes(x= LivingArea, y= Price))+ geom_point()
##--------------------------------------------------------
#TO divide the data into training and test data.
install.packages("caTools")
library(caTools)
dim(housing)
s <- sample.split(housing$LivingArea, SplitRatio = 0.75)
s
housing_train <- filter(housing, s==TRUE)
housing_train
dim(housing_train)
housing_test <- filter(housing, s== FALSE)
dim(housing_test)
#building the model with the training data
training_model <- lm(formula = housing_train$Price~ housing_train$LivingArea)
training_model
testing_model <- predict(training_model, housing_test)
testing_model
error <- housing_train$Price-testing_model
error
qqnorm(error)
