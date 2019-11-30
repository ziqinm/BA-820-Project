options(stringsAsFactors = FALSE)

#EFA
library(factoextra)
library(corrplot)
library(psych)
library(GPArotation)
library(psy)

scaleCol = read_csv("scaleCol.csv")

scaleCol_cor <- cor(scaleCol)
cortest.bartlett(scaleCol_cor, nrow(scaleCol))
KMO(scaleCol_cor)
# MSA = 0.78, looks good
corrplot(scaleCol_cor,
         type="upper",
         diag=F,
         method='color',
         order="hclust")

scaleCol_pca = prcomp(scaleCol, center=TRUE, scale=TRUE)
summary(scaleCol_pca)
fviz_screeplot(scaleCol_pca, addlabels=T, ncp = 20)
# Elbow: 3, 6, 11, 14, 17
num_f = fa.parallel(scaleCol, fm="ml", fa="fa")
# 6 factors
sum(num_f$fa.values >1)
# 2 factors
sum(num_f$fa.values >0.7)
# 3 factors

# Try 3 factors
fa3 = fa(scaleCol, nfactors=3, rotate="oblimin", fm="ml")
print(fa3, cut=.3)

# Remove overlapping variables and variables not in any MLs
scaleCol_c = scaleCol %>% 
  select(-BsmtQual, -GarageFinish, -PoolQC, -Functional, -FireplaceQu,
         -BsmtFinType1, -BsmtFinType2, -OverallCond, -LotShape,
         -Utilities, -LandSlope, -ExterCond)

# Try again
fa3_c = fa(scaleCol_c, nfactors=3, rotate="oblimin", fm="ml")
print(fa3_c, cut=.3)
plot(fa3_c, labels = names(scaleCol_c))
fa.diagram(fa3_c)

scaleCol_c = scaleCol_c %>% 
  select(-BsmtExposure)
fa3_c = fa(scaleCol_c, nfactors=3, rotate="oblimin", fm="ml")
print(fa3_c, cut=.3)
# TFI = 0.99, RMSR = 0.01, RMSEA = 0.03, all Excellent
fa.diagram(fa3_c)

## Decide 3 factors