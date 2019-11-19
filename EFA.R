#EFA
install.packages("factoextra")
library(factoextra)
install.packages("corrplot")
library(corrplot)
install.packages("psych")
library(psych)
install.packages("GPArotation")
library(GPArotation)
scaleCol_cor <- cor(scaleCol)
cortest.bartlett(scaleCol_cor, nrow(scaleCol))
KMO(scaleCol_cor)
corrplot(scaleCol_cor,
         type="upper",
         diag=F,
         method='color',
         order="hclust")
scaleCol_pca = prcomp(scaleCol, center=TRUE, scale=TRUE)
fviz_screeplot(scaleCol_pca, addlabels=T)
num_f = fa.parallel(scaleCol, fm="ml", fa="fa")
fa1 = fa(scaleCol, nfactors=3, rotate="oblimin", fm="ml")
sum(num_f$fa.values >1)
sum(num_f$fa.values >0.7)
class(fa1)
is.list(fa1)
names(fa1)
fa1
print(fa1, cut=.3)