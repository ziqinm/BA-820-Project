options(stringsAsFactors=FALSE)
options(digits=3)


library(tidyverse)
library(factoextra)
library(skimr)
library("corrplot")
library(gridExtra)

train_num = read_csv("train_num.csv")
train_num_s = scale(train_num)

pca_cor = cor(train_num)
corrplot(pca_cor,
         method = "color",
         type = "upper",
         diag = F)

trian_p=prcomp(train_num_s,center=TRUE,scale=TRUE)
class(trian_p)
summary(trian_p)
fviz_screeplot(trian_p,addlabels=T,ncp=15)
# Elbow: 13
get_eigenvalue(trian_p)
eigen = get_eigenvalue(trian_p)
chart = head(eigen, 13)
png("PCA Eigenvalues.png")
eg = tableGrob(chart)
grid.arrange(eg)
dev.off()
# first 12 components with eigenvalue > 1



##based on thses two methods, I would like to choose 12-pca. 







