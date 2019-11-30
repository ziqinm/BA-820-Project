options(stringsAsFactors = FALSE)

library(tidyverse)
library(cluster)
library(factoextra)

type = read_csv("typeCol.csv")
glimpse(type)
summary(type)
type = as.data.frame(type)

num = read_csv("train_num.csv")
num = as.data.frame(num)

scale = read_csv("scaleCol.csv")
scale = as.data.frame(scale)

type8 = type
# factor type variables, not ordered

scale8 = scale
for (i in 1:ncol(scale)) {
  scale8[i] = as.factor(scale[i])
}

new_train = cbind(type8, scale8, num)
dim(new_train)

diss_mat = daisy(new_train, metric = c("gower"))

pam2 = pam(diss_mat, 2)
plot(silhouette(pam2$clustering, diss_mat), col = 1:2, border = NA)

pam3 = pam(diss_mat, 3)
plot(silhouette(pam3$clustering, diss_mat), col = 1:3, border = NA)

pam4 = pam(diss_mat, 4)
plot(silhouette(pam4$clustering, diss_mat), col = 1:4, border = NA)


## Will choose 2
clus = pam2$clustering
length(clus)
all.equal(names(clus), rownames(new_train))
new = cbind(new_train, clus)
glimpse(new)
unique(new$clus)
