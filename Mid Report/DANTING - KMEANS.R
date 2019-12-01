library(dplyr)
library(purrr)
library(cluster)
library(factoextra)

train_num = read_csv("train_num.csv")
train_num_s <- scale(train_num)

fviz_nbclust(train_num_s, kmeans, method="silhouette")
# Top 3: k = 2, 3, 6
fviz_nbclust(train_num_s, kmeans, method = "wss")
# Elbow: k= 4

# 2 centers
kw2 <- kmeans(train_num_s, centers = 2, iter.max = 25, nstart = 25)
fviz_cluster(kw2, data = train_num_s)
kw2$size
plot(silhouette(kw2$cluster, dist = dist(train_num_s)), col = 1:2, border = NA)
# Avg silhouette score: 0.13

# Remove outliers
outlier = as.numeric(c("1299", "54", "524", "636", "186"))
train_num_c = train_num_s[-outlier, ]

fviz_nbclust(train_num_c, kmeans, method="silhouette")
# Top 3: k = 2, 3, 4
fviz_nbclust(train_num_c, kmeans, method = "wss")
# Elbow: k= 4

# Redo 2 centers
kw2_c <- kmeans(train_num_c, centers = 2, iter.max = 25, nstart = 25)
fviz_cluster(kw2_c, data = train_num_c)
kw2_c$size
plot(silhouette(kw2_c$cluster, dist = dist(train_num_c)), col = 1:2, border = NA)
# Avg silhouette score: 0.14

kw3 <- kmeans(train_num_c, centers = 3, 25, 25)
fviz_cluster(kw3, data = train_num_c)
plot(silhouette(kw3$cluster, dist = dist(train_num_c)), col = 1:3, border = NA)
# Avg silhouette score: 0.13

kw4 <- kmeans(train_num_c, centers = 4, 25, 25)
fviz_cluster(kw4, data = train_num_c)
plot(silhouette(kw4$cluster, dist = dist(train_num_c)), col = 1:4, border = NA)
# Avg silhouette score: 0.14
kw4$size

kw6 = kmeans(train_num_c, centers = 6, 25, 25)
fviz_cluster(kw6, data = train_num_c)
plot(silhouette(kw6$cluster, dist = dist(train_num_c)), col = 1:6, border = NA)
# Avg silhouette score: 0.11

## Will choose k = 2 for now


