library(dplyr)
library(purrr)
library(cluster)
library(factoextra)
hclust(train_num)
kw <- kmeans(train_num, centers = 2, iter.max = 25, nstart = 25)
fviz_cluster(kw, data = train_num)
is.na(train_num)
kw$size
train_num_s <- scale(train_num)
dist_num <- dist(train_num_s)
h1 <- hcut(dist_num, k=4)
fviz_dend(h1, rect = TRUE)

##optimal number of clusters = 2
fviz_nbclust(train_num, kmeans, method="silhouette")

