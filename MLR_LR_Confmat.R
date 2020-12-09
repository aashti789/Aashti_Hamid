housing <-  read.csv('C:/R dataset/Housing_data.csv')
View(housing)
library(caTools)
s <-  sample.split(housing$Price, SplitRatio = 0.7)
library(dplyr)
housing_Train <-  filter(housing, s== TRUE)
housing_Train
housing_Test <-  filter(housing, s== FALSE)
housing_Test
#Building the model using multilinear regression.
housing_MLR_model <-  lm(Price~., data= housing_Train)
housing_MLR_model
summary(housing_MLR_model)
#Modelling equation is: Price= 31607.690 + 66.005*LivingArea+15158.563*Bathrooms-253.649*Age
#Accuracy of the model.
Predict_Price <-  predict(housing_MLR_model, housing_Test)
Predict_Price
error <-  housing_test$Price - Predict_Price
error
qqnorm(error)
summary(error)
#Principal Component Analysis
library(car)
vif(housing_MLR_model)
cor(housing)
house <- housing[-1]
pc <- princomp(house, cor= TRUE, score= TRUE)
pc
summary(pc)
plot(pc)
biplot(pc)
pc$loadings
pc$scores
##Classification:Linear: Logistic REgression when the output is binary(0,1)and integer.
admission <-  read.csv('C:/R dataset/admission.csv')
View(admission)
str(admission)
library(caTools)
library(dplyr)
s <- sample.split(admission$GRE, SplitRatio = 0.7)
admission_Train <- filter(admission, s== TRUE)
admission_Test <- filter(admission, s== FALSE)
#Building the logistic Regression Model
admission_LR_model <- glm(ADMIT~., data= admission_Train, family = 'binomial')
summary(admission_LR_model)
#Equation: Price= -4.325162-0.5321051*Rank
predict_admit <- predict(admission_LR_model, admission_Test)
predict_admit
error <- admission_Test$ADMIT-predict_admit
error
qqnorm(error)
install.packages('caret')
library(caret)
dim(predict_admit)
dim(admission_Test$ADMIT)
predict_admit1 <- ifelse(predict_admit>0.5,1,0)
confusionMatrix(table(predict_admit1, admission_Test$ADMIT))
#PPV= True positive/(true positive+false positive)
#NPV= True Negative/(false negative+false positive)
#Sensitivity= True positive/(true positive+ false negative)
#Specificity= True negative/(true negative+ false positive)