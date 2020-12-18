##Using stringsasFactors to convert all the characters into factors.
banking <- read.csv('C:/R dataset/Banking.csv',stringsAsFactors = TRUE)
View(banking)
str(banking)
cluster_model <- kmeans(banking, centers = 4, iter.max = 10)
#NaNs are produced in the output as in clustering integers are used
#Converting all the factor variables into integers.
banking$job <- as.integer(banking$job)
str(banking)
banking$marital <- as.integer(banking$marital)
banking$education <- as.integer(banking$education)
banking$default <- as.integer(banking$default)
banking$housing <- as.integer(banking$housing)
banking$loan <- as.integer(banking$loan)
banking$contact <- as.integer(banking$contact )
banking$month <- as.integer(banking$month)
banking$poutcome <- as.integer(banking$poutcome)
str(banking)

#Omiting all the rows in which nans are there.
banking <- na.omit(banking)
#Scaling the input.
banking <- scale(banking)
View(banking)

#Now clustering the data.
cluster_model <- kmeans(banking, centers = 4, iter.max = 10)
cluster_model
cluster_model$cluster
banking1 <- cbind(banking, cluster_model$cluster)
View(banking1)

##To get the optimum number of centers we use elbow curve.
install.packages('factoextra')
library(factoextra)
fviz_nbclust(banking, kmeans, method='wss')
fviz_nbclust(banking, kmeans, method='silhouette')

#From the elbow curve we get optimum number of centers=3
#From silhouette curve we find out that obs are quite incorrectly assigned
#as the avg silhouette width is near to zero.
#Taking centers=3.
cluster_model <- kmeans(banking, centers = 3, iter.max = 10)
cluster_model$cluster
banking2 <- cbind(banking, cluster_model$cluster)
View(banking2)
#graph and count of expected clusters
install.packages('mclust')
library(mclust)
fit <- Mclust(banking2)
plot(fit)

