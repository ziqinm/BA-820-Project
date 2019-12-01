options(stringsAsFactors=FALSE)
options(digits=3)


library(tidyverse)
library(factoextra)
library(skimr)
library("corrplot")
library(gridExtra)

train_num = read_csv("Subsets/train_dbl.csv")
summary(train_num)
train_num_s = scale(train_num)

pca_cor = cor(train_num)
corrplot(pca_cor,
         method = "color",
         type = "upper",
         diag = F)

train_p=prcomp(train_num_s,center=TRUE,scale=TRUE)
summary(train_p)
fviz_screeplot(train_p,addlabels=T, ncp = 25)
# Elbow: 11, 16, 19, 22
get_eigenvalue(train_p)
# first 11 components with eigenvalue > 1
# 22 pcs explains ~93% variance, will choose 22
eigen = get_eigenvalue(train_p)
chart = head(eigen, 23)
png("PCA Eigenvalues.png")
eg = tableGrob(chart)
grid.arrange(eg)
dev.off()


##### Max/min scale #####




##### Profiling pcs for predictions #####

train_pca = predict(train_p, newdata = train_num)







