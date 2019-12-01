
library(tidyverse)
library(cluster)
library(factoextra)


type = read_csv("Subsets/type_chr.csv")
outlier = as.numeric(c("1299", "54", "524", "636", "186"))
type = type[-outlier, ]
glimpse(type)
summary(type)
type = as.data.frame(type)

num = read_csv("Subsets/train_dbl_c.csv")
num = as.data.frame(num)

scale = read_csv("Subsets/scale_fct.csv")
scale = scale[-outlier, ]
scale = as.data.frame(scale)

# factor type variables, not ordered
typeFct = type %>% mutate_if(is.character, as.factor)



new_train = cbind(typeFct, scale, num)
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




dema = cbind(new_train = cbind(type8, scaleFct, num))
diss_mat_dema = daisy(new_train, metric = c("gower"))
pam2d = pam(diss_mat_dema, 2)
plot(silhouette(pam2d$clustering, diss_mat_dema), col = 1:2, border = NA)
