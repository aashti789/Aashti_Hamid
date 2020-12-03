#logistic regression is only for binary outputs like yes/No, TRUE/ FAlse, 1/0.
admit <- read.csv('C:/R dataset/admission.csv')
View(admit)
str(admit)
dim(admit)
#Building Linear Regression Model.
library(caTools)
s <- sample.split(admit$GRE, SplitRatio = 0.7)
s
library(dplyr)
admit_train <- filter(admit, s== TRUE)
admit_train
admit_test <- filter(admit, s== FALSE)
admit_test
dim(admit_test)
dim(admit_train)

#logistic regression, when the output have only two values
admit_model_logistic <- glm(formula = ADMIT ~., data = admit, family = 'binomial')
admit_model_logistic
summary(admit_model_logistic)

#Analizing the model only rank is significant.
#To check the multi-collinearity
library(car)
b <- vif(admit_model_logistic)
b
cor(admit)