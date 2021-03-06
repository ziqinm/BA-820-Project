options(stringsAsFactors = FALSE)

library(dplyr)
library(purrr)
library(cluster)
library(factoextra)

train_num = read_csv("Subsets/train_dbl.csv")
train_num_s <- scale(train_num)

fviz_nbclust(train_num_s, kmeans, method="silhouette", k.max = 33)
# Top 3: k = 2, 3, 4
fviz_nbclust(train_num_s, kmeans, method = "wss", k.max = 33)
# Elbow: k= 4

# 2 centers
kw2 <- kmeans(train_num_s, centers = 2, iter.max = 25, nstart = 25)
fviz_cluster(kw2, data = train_num_s)
kw2$size
plot(silhouette(kw2$cluster, dist = dist(train_num_s)), col = 1:2, border = NA)
# Avg silhouette score: 0.14

# Remove outliers to get a better visuailization
outlier = as.numeric(c("1299", "54", "524", "636", "186"))
train_num_c = train_num_s[-outlier, ]
train_num_c = as.data.frame(train_num_c)

fviz_nbclust(train_num_c, kmeans, method="silhouette")
# Top 3: k = 2, 3, 9
fviz_nbclust(train_num_c, kmeans, method = "wss")
# Elbow: k= 4, 7

# Redo 2 centers
kw2_c <- kmeans(train_num_c, centers = 2, iter.max = 25, nstart = 25)
fviz_cluster(kw2_c, data = train_num_c)
kw2_c$size
plot(silhouette(kw2_c$cluster, dist = dist(train_num_c)), col = 1:2, border = NA)
# Avg silhouette score: 0.14

kw3 <- kmeans(train_num_c, centers = 3, 25, 25)
fviz_cluster(kw3, data = train_num_c)
plot(silhouette(kw3$cluster, dist = dist(train_num_c)), col = 1:3, border = NA)
# Avg silhouette score: 0.14

kw4 <- kmeans(train_num_c, centers = 4, 25, 25)
fviz_cluster(kw4, data = train_num_c)
plot(silhouette(kw4$cluster, dist = dist(train_num_c)), col = 1:4, border = NA)
# Avg silhouette score: 0.15
kw4$size
kw4$cluster

kw7 = kmeans(train_num_c, centers = 7, 25, 25)
fviz_cluster(kw7, data = train_num_c)
plot(silhouette(kw7$cluster, dist = dist(train_num_c)), col = 1:7, border = NA)
# Avg silhouette score: 0.11

kw9 = kmeans(train_num_c, centers = 9, 25, 25)
fviz_cluster(kw9, data = train_num_c)
plot(silhouette(kw9$cluster, dist = dist(train_num_c)), col = 1:9, border = NA)
# Avg silhouette score: 0.11

## Will choose k = 4

ndf = train_num[-outlier, ]
write_csv(ndf, "Subsets/train_n.csv")
