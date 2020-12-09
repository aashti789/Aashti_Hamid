View(iris)
library(e1071)
library(caTools)
set.seed(100)
s <-  sample.split(iris$Petal.Width,SplitRatio = 0.7) 
library(dplyr)
iris_train <- filter(iris, s== TRUE)
dim(iris_train)
str(iris_train)
iris_test <- filter(iris, s== FALSE)
dim(iris_test)

iris_lin_model <- svm(Species~ Petal.Length+Petal.Width, data=iris, kernel='linear', cost=1)
iris_lin_model
summary(iris_lin_model)

iris_rad_model <- svm(Species~ Petal.Length+Petal.Width, data= iris, kernel= 'radial', cost=1)
summary(iris_rad_model)

plot(iris_lin_model, iris[c(5,3,4)]); legend("center", "linear")
plot(iris_rad_model, iris[c(5,3,4)]); legend("center", "radial")

pred_lin <- predict(iris_lin_model, iris_test)
summary(pred_lin)
library(caret)
confusionMatrix(table(iris_test$Species, pred_lin))

pred_rad <- predict(iris_rad_model, iris_test)
confusionMatrix(iris_test$Species, pred_rad)
#SVM model using linear or radial as kernels gave exactly same results.