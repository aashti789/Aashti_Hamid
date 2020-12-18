banking <-  read.csv('C:/R dataset/banking.csv', stringsAsFactors = TRUE)
View(banking)
str(banking)#To kn0w the structure of the data.
summary(banking)#To know the mean, median, mode
#Converting all the character variables into integer variables.
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

#scaling the data.
banking <- scale(banking)
View(banking)

#Using Hierarchical Clustering.
distance <- dist(banking, method = "euclidean")
distance
h_clust <- hclust(distance, method = "ward.D")
h_clust
plot(h_clust)
rect.hclust(h_clust,4)

groups <- cutree(h_clust,4)
groups
banking1 <- cbind(banking, groups)
View(banking1)
##To know the optimum number of groups, elbow curve.
library(factoextra)
fviz_nbclust(banking,kmeans, method = 'wss')
fviz_nbclust(banking, kmeans, method = "silhouette")
h_clust
##Lets take the optimum number of clusters as 2.
groups1 <- cutree(h_clust,2)
banking2 <- cbind(banking,groups1)
banking2
