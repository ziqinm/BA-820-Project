options(stringsAsFactors = FALSE)

library(tidyverse)
library(cluster)
library(factoextra)
library(fpc)


type = read.csv("Subsets/type_chr.csv")
outlier = as.numeric(c("1299", "54", "524", "636", "186"))
type = type[-outlier, ]
glimpse(type)
summary(type)
# type = as.data.frame(type)

num = read.csv("Subsets/train_n.csv")
# num = as.data.frame(num)

# scale = read_csv("Subsets/scale_fct.csv")
# scale = scale[-outlier, ]
# scale = as.data.frame(scale)

# factor type variables, not ordered
typeFct = type %>% mutate_if(is.character, as.factor)

## Cheating from DataCleaning File!!
scale8 = scaleFct[-outlier, ]

new_train = cbind(typeFct, scale8, num)
dim(new_train)
glimpse(new_train)

##### PAM algorithm #####

diss_dist = daisy(new_train, metric = c("gower"))

sil_width <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(diss_dist, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}
plot(1:8, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:8, sil_width)
# k = 2 has the highest silhouette width

pam2 = pam(diss_dist, 2)
pam2$id.med
plot(silhouette(pam2$clustering, diss_dist), col = 1:2, border = NA)
# Avg: 0.2

## Will choose 2


##### Profile back #####

clus = pam2$clustering
length(clus)
all.equal(names(clus), rownames(new_train))

train_pca = read.csv("Model/train_pca.csv")
de = train_orig[-outlier, ]
price = de$SalePrice

train_df = cbind(train_pca, clus, price)
dim(train_df)
write_csv(train_df, "Model/train_df.csv")

# new = cbind(new_train, clus)
# glimpse(new)
# unique(new$clus)
