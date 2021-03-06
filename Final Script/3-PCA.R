options(stringsAsFactors=FALSE)


library(tidyverse)
library(factoextra)
library(skimr)
library("corrplot")
library(gridExtra)

train_num = read.csv("Subsets/train_n.csv")
summary(train_num)

pca_cor = cor(train_num)
corrplot(pca_cor,
         method = "color",
         type = "upper",
         diag = F)

train_num_s = scale(train_num)

train_p=prcomp(train_num_s,center=TRUE,scale=TRUE)
summary(train_p)
fviz_screeplot(train_p,addlabels=T, ncp = 25)
# Elbow: 11, 16, 19, 22
get_eigenvalue(train_p)
# first 10 components with eigenvalue > 1
# 22 pcs explains ~93% variance, will choose 22

# eigen = get_eigenvalue(train_p)
# chart = head(eigen, 23)
# png("PCA Eigenvalues.png")
# eg = tableGrob(chart)
# grid.arrange(eg)
# dev.off()

train_pca = predict(train_p, newdata = train_num_s)
train_pca = train_pca[, 1:22]
train_pca = as.data.frame(train_pca)
nrow(train_pca) == nrow(train_num)

write.csv(train_pca, "Model/train_pca.csv")

save(train_p, file = "Model/pca.rda")

##### Max/min scale #####
# 
# normalize <- function(x) {
#   return ((x - min(x)) / (max(x) - min(x)))
# }
# 
# train_num_N <- as.data.frame(lapply(train_num, normalize))
# summary(train_num_N)
# 
# train_N_p=prcomp(train_num_N,center=TRUE,scale=TRUE)
# summary(train_N_p)
# fviz_screeplot(train_N_p,addlabels=T, ncp = 25)
# # Elbow: 11, 16, 19, 22
# get_eigenvalue(train_N_p)
# # first 10 components with eigenvalue > 1
# # 22 pcs explains ~93% variance, will choose 22
# eigen_N = get_eigenvalue(train_N_p)
# chart_N = head(eigen_N, 23)
# png("PCA Eigenvalues.png")
# eg_N = tableGrob(chart_N)
# grid.arrange(eg_N)
# dev.off()
# 
# Profiling pcs for predictions

# train_pca = predict(train_N_p, newdata = train_num_N)
# train_pca = train_pca[, 1:22]
# train_pca = as.data.frame(train_pca)
# nrow(train_pca) == nrow(train_num)



