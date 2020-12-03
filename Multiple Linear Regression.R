#Multivariable Regression
housing_data <- read.csv('C:/R dataset/Housing_data.csv')
View(housing_data)
library(caTools)
s <- sample.split(housing_data$Price, SplitRatio = 0.7)
s
library(dplyr)
housing_Traing <- filter(housing_data, s== TRUE)
housing_Traing
housing_Test <- filter(housing_data, s== FALSE)
housing_Test
#MOdel
housing_model <- lm(Price~., data= housing_Traing)
housing_model
summary(housing_model)
#Leaving Bedrooms and lotsize all other  variables are statistically significant.
predict_price <- predict(housing_model, housing_Test)
predict_price
error <- housing_test$Price- predict_price
qqnorm(error)
#checking the multi-collinearity.
install.packages('car')
library(car)
cor(housing_data)#correlation coefficients
vif(housing_model)
#if vif values are found greater than 10 then do PCA analysis on the data set
#then consider those components only whose summation of variance is greater than 90%
#then make another data set using scores, and now use this data set and again
#dividing training and test data sets make a model.
a<-princomp(~.,data = housing_data)
summary(a)
loadings(a)
b <- a$scores
b
plot(a, type='l') #PLotting elbow curve

