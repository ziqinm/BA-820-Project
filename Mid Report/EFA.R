options(stringsAsFactors = FALSE)

#EFA
library(factoextra)
library(corrplot)
library(psych)
library(GPArotation)
library(psy)

scale = read_csv("scale_fct.csv")
glimpse(scale)

## Convert scales to numeric
scaleCol = scale
## Systematically factored:
# LotShape: Reg = 4, IR1 = 3, IR2 = 2, IR1 = 1
scaleCol$LotShape = as.numeric(factor(scaleCol$LotShape))
scaleCol$LotShape = replace_na(scaleCol$LotShape, 0)
# LandSlope: Gtl = 1, Mod = 2, Sev = 3
scaleCol$LandSlope = as.numeric(factor(scaleCol$LandSlope))
scaleCol$LandSlope = replace_na(scaleCol$LandSlope, 0)
# ExterQual: Ex = 1, Gd = 3, TA = 4, Fa = 2
scaleCol$ExterQual = as.numeric(factor(scaleCol$ExterQual))
scaleCol$ExterQual = replace_na(scaleCol$ExterQual, 0)
# ExterCond: TA = 5, Gd = 3, Fa = 2, Po = 4, Ex = 1
scaleCol$ExterCond = as.numeric(factor(scaleCol$ExterCond))
scaleCol$ExterCond = replace_na(scaleCol$ExterCond, 0)
# BsmtQual: Ex = 1, TA = 4, Gd = 3, FA = 2, NA = 0
scaleCol$BsmtQual = as.numeric(factor(scaleCol$BsmtQual))
scaleCol$BsmtQual = replace_na(scaleCol$BsmtQual, 0)
# BsmtCond: TA = 4, Gd = 2, FA = 1, Po = 3, NA = 0
scaleCol$BsmtCond = as.numeric(factor(scaleCol$BsmtCond))
scaleCol$BsmtCond = replace_na(scaleCol$BsmtCond, 0)
# BsmtExposure: No = 4, Mn = 3, Gd = 2, Av = 1, NA = 0
scaleCol$BsmtExposure = as.numeric(factor(scaleCol$BsmtExposure))
scaleCol$BsmtExposure = replace_na(scaleCol$BsmtExposure, 0)
# HeatingQC: Ex = 1, Gd = 3, TA = 5, Fa = 2, Po = 4
scaleCol$HeatingQC = as.numeric(factor(scaleCol$HeatingQC))
scaleCol$HeatingQC = replace_na(scaleCol$HeatingQC, 0)
# CentralAir: Y = 2, N = 1
scaleCol$CentralAir = as.numeric(factor(scaleCol$CentralAir))
scaleCol$CentralAir = replace_na(scaleCol$CentralAir, 0)
# KitchenQual:  Ex = 1, TA = 4, Gd = 3, FA = 2
scaleCol$KitchenQual = as.numeric(factor(scaleCol$KitchenQual))
scaleCol$KitchenQual = replace_na(scaleCol$KitchenQual, 0)
# FireplaceQu: Ex = 1, Gd = 3, TA = 5, Fa = 2, Po = 4, NA = 0
scaleCol$FireplaceQu = as.numeric(factor(scaleCol$FireplaceQu))
scaleCol$FireplaceQu = replace_na(scaleCol$FireplaceQu, 0)
# GarageQual: Ex = 1, Gd = 3, TA = 5, Fa = 2, Po = 4, NA = 0
scaleCol$GarageQual = as.numeric(factor(scaleCol$GarageQual))
scaleCol$GarageQual = replace_na(scaleCol$GarageQual, 0)
# GarageCond: Ex = 1, Gd = 3, TA = 5, Fa = 2, Po = 4, NA = 0
scaleCol$GarageCond = as.numeric(factor(scaleCol$GarageCond))
scaleCol$GarageCond = replace_na(scaleCol$GarageCond, 0)
# PoolQC: Ex = 1, Fa = 2, Gd = 3, NA = 0
scaleCol$PoolQC = as.numeric(factor(scaleCol$PoolQC))
scaleCol$PoolQC = replace_na(scaleCol$PoolQC, 0)

## Manually factored:
# Utilities: AllPub = 4, NoSewr = 3, NoSeWa = 2, ELO = 1
scaleCol$Utilities = gsub("AllPub", 4, scaleCol$Utilities)
scaleCol$Utilities = gsub("NoSewr", 3, scaleCol$Utilities)
scaleCol$Utilities = gsub("NoSeWa", 2, scaleCol$Utilities)
scaleCol$Utilities = gsub("ELO", 1, scaleCol$Utilities)
scaleCol$Utilities = as.numeric(scaleCol$Utilities)
scaleCol$Utilities = replace_na(scaleCol$Utilities, 0)
# BsmtFinType1: GLQ = 6, ALQ = 5, BLQ = 4, Rec = 3, LwQ = 2, Unf = 1, NA = 0
scaleCol$BsmtFinType1 = gsub("GLQ", 6, scaleCol$BsmtFinType1)
scaleCol$BsmtFinType1 = gsub("ALQ", 5, scaleCol$BsmtFinType1)
scaleCol$BsmtFinType1 = gsub("BLQ", 4, scaleCol$BsmtFinType1)
scaleCol$BsmtFinType1 = gsub("Rec", 3, scaleCol$BsmtFinType1)
scaleCol$BsmtFinType1 = gsub("LwQ", 2, scaleCol$BsmtFinType1)
scaleCol$BsmtFinType1 = gsub("Unf", 1, scaleCol$BsmtFinType1)
scaleCol$BsmtFinType1 = as.numeric(scaleCol$BsmtFinType1)
scaleCol$BsmtFinType1 = replace_na(scaleCol$BsmtFinType1, 0)
# BsmtFinType2: GLQ = 6, ALQ = 5, BLQ = 4, Rec = 3, LwQ = 2, Unf = 1, NA = 0
scaleCol$BsmtFinType2 = gsub("GLQ", 6, scaleCol$BsmtFinType2)
scaleCol$BsmtFinType2 = gsub("ALQ", 5, scaleCol$BsmtFinType2)
scaleCol$BsmtFinType2 = gsub("BLQ", 4, scaleCol$BsmtFinType2)
scaleCol$BsmtFinType2 = gsub("Rec", 3, scaleCol$BsmtFinType2)
scaleCol$BsmtFinType2 = gsub("LwQ", 2, scaleCol$BsmtFinType2)
scaleCol$BsmtFinType2 = gsub("Unf", 1, scaleCol$BsmtFinType2)
scaleCol$BsmtFinType2 = as.numeric(scaleCol$BsmtFinType2)
scaleCol$BsmtFinType2 = replace_na(scaleCol$BsmtFinType2, 0)
# Electrical: SBrkr = 5, FuseA = 4, FuseF = 3, FuseP = 2, Mix = 1, NA = 0
scaleCol$Electrical = gsub("SBrkr", 5, scaleCol$Electrical)
scaleCol$Electrical = gsub("FuseA", 4, scaleCol$Electrical)
scaleCol$Electrical = gsub("FuseF", 3, scaleCol$Electrical)
scaleCol$Electrical = gsub("FuseP", 2, scaleCol$Electrical)
scaleCol$Electrical = gsub("Mix", 1, scaleCol$Electrical)
scaleCol$Electrical = as.numeric(scaleCol$Electrical)
scaleCol$Electrical = replace_na(scaleCol$Electrical, 0)
# Functional: Typ = 8, Min1 = 7, Min2 = 6, Mod = 5, Maj1 = 4, Maj2 = 3, Sev = 2, Sal = 1
scaleCol$Functional = gsub("Typ", 8, scaleCol$Functional)
scaleCol$Functional = gsub("Min1", 7, scaleCol$Functional)
scaleCol$Functional = gsub("Min2", 6, scaleCol$Functional)
scaleCol$Functional = gsub("Mod", 5, scaleCol$Functional)
scaleCol$Functional = gsub("Maj1", 4, scaleCol$Functional)
scaleCol$Functional = gsub("Maj2", 3, scaleCol$Functional)
scaleCol$Functional = gsub("Sev", 2, scaleCol$Functional)
scaleCol$Functional = gsub("Sal", 1, scaleCol$Functional)
scaleCol$Functional = as.numeric(scaleCol$Functional)
scaleCol$Functional = replace_na(scaleCol$Functional, 0)
# GarageFinish: RFn = 3, Fin = 2, Unf = 1, NA = 0
scaleCol$GarageFinish = gsub("RFn", 3, scaleCol$GarageFinish)
scaleCol$GarageFinish = gsub("Fin", 2, scaleCol$GarageFinish)
scaleCol$GarageFinish = gsub("Unf", 1, scaleCol$GarageFinish)
scaleCol$GarageFinish = as.numeric(scaleCol$GarageFinish)
scaleCol$GarageFinish = replace_na(scaleCol$GarageFinish, 0)
# PavedDrive: Y = 3, P = 2, N = 1
scaleCol$PavedDrive = gsub("Y", 3, scaleCol$PavedDrive)
scaleCol$PavedDrive = gsub("P", 2, scaleCol$PavedDrive)
scaleCol$PavedDrive = gsub("N", 1, scaleCol$PavedDrive)
scaleCol$PavedDrive = as.numeric(scaleCol$PavedDrive)
scaleCol$PavedDrive = replace_na(scaleCol$PavedDrive, 0)

# Make an copy
write_csv(scaleCol, "scale_EFA.csv")

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