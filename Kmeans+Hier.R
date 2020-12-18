grades <- read.csv('C:/R dataset/Demo 2_ Perform hierarchical clustering.csv')
View(grades)
str(grades)
summary(grades)
grades <- scale(grades)
#Using Kmeans Clustering.
library(mclust)
grades_kmeans <- kmeans(grades, centers = 4, iter.max = 10)#Assuming centers=4
grades_kmeans
grades_kmeans$cluster
grades_kmeans1 <- cbind(grades, grades_kmeans$cluster)
View(grades_kmeans1)
##Finding the optimum number of centers, using the elbow curve.
library(factoextra)
fviz_nbclust(grades, kmeans, method = "wss")
fviz_nbclust(grades, kmeans, method = "silhouette")

##According to elbow curve optimum  number of clusters should be 2 or 3. Taking number of clusters=2.
grades_kmeans2 <- kmeans(grades, centers = 2, iter.max = 10)
grades_kmeans2 <- cbind(grades, grades_kmeans2$cluster)
View(grades_kmeans2)
##PLotting the graphs using mclust.

library(mclust)
fit <- Mclust(grades_kmeans2)
plot(fit)

#Hierarchical Clustering
distance <- dist(grades, method = "euclidean")
h_clust <- hclust(distance, method = "ward.D")
h_clust$merge
plot(h_clust)
h_clust$call
groups <- cutree(h_clust, 2)
plot(groups)

rect.hclust(h_clust,2)
