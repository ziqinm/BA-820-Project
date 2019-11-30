options(stringsAsFactors = FALSE)

# Refer:
# https://www.kaggle.com/erikbruin/house-prices-lasso-xgboost-and-a-detailed-eda


# ----------------------------- #

library(tidyverse)
library(skimr)

train_orig = read_csv("train.csv")
summary(train_orig)
glimpse(train_orig)
skim(train_orig)

test_orig = read_csv("test.csv")

# ----------------------------- #

##### Data Cleaning #####

# Remove `SalePrice` column
train = train_orig %>% 
  select(-SalePrice)
dim(train)
# Move `Id` to rownames, then delete
rownames(train) = train$Id
train = train %>% select(-Id)



### Extract text columns ###
chrName = c("MSZoning", "Street", "LotShape", "LandContour", "Utilities",
             "LotConfig", "LandSlope", "Neighborhood", "Condition1", "Condition2",
             "BldgType", "HouseStyle", "RoofStyle", "RoofMatl", "Exterior1st",
             "Exterior2nd", "MasVnrType", "ExterQual", "ExterCond", "Foundation",
             "BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2",
             "Heating", "HeatingQC", "CentralAir", "Electrical", "KitchenQual",
             "Functional", "FireplaceQu", "GarageType", "GarageFinish", "GarageQual",
             "GarageCond", "PavedDrive", "PoolQC", "Fence", "MiscFeature",
             "SaleType", "SaleCondition", "Alley")
chrCol = train %>% select(chrName)
glimpse(chrCol)

categName = c("MSZoning", "Street", "LandContour", "LotConfig", "Neighborhood", "Condition1",
             "Condition2", "BldgType", "HouseStyle", "RoofStyle", "RoofMatl", "Exterior1st",
             "Exterior2nd", "MasVnrType", "Foundation", "Heating", "GarageType",
             "SaleType", "SaleCondition", "Alley", "Fence", "MiscFeature")

typeCol = chrCol %>% 
  select(categName)

scaleCol = chrCol %>% 
  select(-categName)

### Convert rank categories to scales for EFA ###
## Systematically factored:
# LotShape: Reg = 4, IR1 = 3, IR2 = 2, IR1 = 1
scaleCol$LotShape = as.numeric(factor(scaleCol$LotShape))
# LandDlope: Gtl = 1, Mod = 2, Sev = 3
scaleCol$LandSlope = as.numeric(factor(scaleCol$LandSlope))
# ExterQual: Ex = 1, Gd = 3, TA = 4, Fa = 2
scaleCol$ExterQual = as.numeric(factor(scaleCol$ExterQual))
# ExterCond: TA = 5, Gd = 3, Fa = 2, Po = 4, Ex = 1
scaleCol$ExterCond = as.numeric(factor(scaleCol$ExterCond))
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
# CentralAir: Y = 2, N = 1
scaleCol$CentralAir = as.numeric(factor(scaleCol$CentralAir))
# KitchenQual:  Ex = 1, TA = 4, Gd = 3, FA = 2
scaleCol$KitchenQual = as.numeric(factor(scaleCol$KitchenQual))
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
# unique(scaleCol$Utilities)
scaleCol$Utilities = gsub("AllPub", 4, scaleCol$Utilities)
scaleCol$Utilities = gsub("NoSeWa", 2, scaleCol$Utilities)
scaleCol$Utilities = as.numeric(scaleCol$Utilities)
# BsmtFinType1: GLQ = 6, ALQ = 5, BLQ = 4, Rec = 3, LwQ = 2, Unf = 1, NA = 0
# unique(scaleCol$BsmtFinType1)
scaleCol$BsmtFinType1 = gsub("GLQ", 6, scaleCol$BsmtFinType1)
scaleCol$BsmtFinType1 = gsub("ALQ", 5, scaleCol$BsmtFinType1)
scaleCol$BsmtFinType1 = gsub("BLQ", 4, scaleCol$BsmtFinType1)
scaleCol$BsmtFinType1 = gsub("Rec", 3, scaleCol$BsmtFinType1)
scaleCol$BsmtFinType1 = gsub("LwQ", 2, scaleCol$BsmtFinType1)
scaleCol$BsmtFinType1 = gsub("Unf", 1, scaleCol$BsmtFinType1)
scaleCol$BsmtFinType1 = as.numeric(scaleCol$BsmtFinType1)
scaleCol$BsmtFinType1 = replace_na(scaleCol$BsmtFinType1, 0)
# BsmtFinType2: GLQ = 6, ALQ = 5, BLQ = 4, Rec = 3, LwQ = 2, Unf = 1, NA = 0
# unique(scaleCol$BsmtFinType2)
scaleCol$BsmtFinType2 = gsub("GLQ", 6, scaleCol$BsmtFinType2)
scaleCol$BsmtFinType2 = gsub("ALQ", 5, scaleCol$BsmtFinType2)
scaleCol$BsmtFinType2 = gsub("BLQ", 4, scaleCol$BsmtFinType2)
scaleCol$BsmtFinType2 = gsub("Rec", 3, scaleCol$BsmtFinType2)
scaleCol$BsmtFinType2 = gsub("LwQ", 2, scaleCol$BsmtFinType2)
scaleCol$BsmtFinType2 = gsub("Unf", 1, scaleCol$BsmtFinType2)
scaleCol$BsmtFinType2 = as.numeric(scaleCol$BsmtFinType2)
scaleCol$BsmtFinType2 = replace_na(scaleCol$BsmtFinType2, 0)
# Electrical: SBrkr = 5, FuseA = 4, FuseF = 3, FuseP = 2, Mix = 1, NA = 0
# unique(scaleCol$Electrical)
scaleCol$Electrical = gsub("SBrkr", 5, scaleCol$Electrical)
scaleCol$Electrical = gsub("FuseA", 4, scaleCol$Electrical)
scaleCol$Electrical = gsub("FuseF", 3, scaleCol$Electrical)
scaleCol$Electrical = gsub("FuseP", 2, scaleCol$Electrical)
scaleCol$Electrical = gsub("Mix", 1, scaleCol$Electrical)
scaleCol$Electrical = as.numeric(scaleCol$Electrical)
scaleCol$Electrical = replace_na(scaleCol$Electrical, 0)
# Functional: Typ = 8, Min1 = 7, Min2 = 6, Mod = 5, Maj1 = 4, Maj2 = 3, Sev = 2, Sal = 1
# unique(scaleCol$Functional)
scaleCol$Functional = gsub("Typ", 8, scaleCol$Functional)
scaleCol$Functional = gsub("Min1", 7, scaleCol$Functional)
scaleCol$Functional = gsub("Min2", 6, scaleCol$Functional)
scaleCol$Functional = gsub("Mod", 5, scaleCol$Functional)
scaleCol$Functional = gsub("Maj1", 4, scaleCol$Functional)
scaleCol$Functional = gsub("Maj2", 3, scaleCol$Functional)
scaleCol$Functional = gsub("Sev", 2, scaleCol$Functional)
scaleCol$Functional = gsub("Sal", 1, scaleCol$Functional)
scaleCol$Functional = as.numeric(scaleCol$Functional)
# GarageFinish: RFn = 3, Fin = 2, Unf = 1, NA = 0
# unique(scaleCol$GarageFinish)
scaleCol$GarageFinish = gsub("RFn", 3, scaleCol$GarageFinish)
scaleCol$GarageFinish = gsub("Fin", 2, scaleCol$GarageFinish)
scaleCol$GarageFinish = gsub("Unf", 1, scaleCol$GarageFinish)
scaleCol$GarageFinish = as.numeric(scaleCol$GarageFinish)
scaleCol$GarageFinish = replace_na(scaleCol$GarageFinish, 0)
# PavedDrive: Y = 3, P = 2, N = 1
# unique(scaleCol$PavedDrive)
scaleCol$PavedDrive = gsub("Y", 3, scaleCol$PavedDrive)
scaleCol$PavedDrive = gsub("P", 2, scaleCol$PavedDrive)
scaleCol$PavedDrive = gsub("N", 1, scaleCol$PavedDrive)
scaleCol$PavedDrive = as.numeric(scaleCol$PavedDrive)



           
# Extraxt other numeric scale variables
train %>% select(-chrName) %>% glimpse()
scaleName = c("OverallQual", "OverallCond")
scaleNum = train %>% select(scaleName)
scaleCol = cbind(scaleCol, scaleNum)
glimpse(scaleCol)
skim(scaleCol)
which(is.na(scaleCol), arr.ind=TRUE)   
# row 1380 has NA, decided to add NA as a category
train %>% select(-chrName, -scaleName) %>% glimpse() -> train_num


### Missing Values ###
skim(typeCol)
# Replace NAs with "N/A" as a new type
typeCol$Alley = replace_na(typeCol$Alley, "N/A")
typeCol$Fence = replace_na(typeCol$Fence, "N/A")
typeCol$GarageType = replace_na(typeCol$GarageType, "N/A")
typeCol$MasVnrType = replace_na(typeCol$MasVnrType, "N/A")
typeCol$MiscFeature = replace_na(typeCol$MiscFeature, "N/A")
skim(typeCol)


# Numeric variables: Replace with mean
skim(train_num)
summary(train_num)
train_num$LotFrontage = replace_na(train_num$LotFrontage,
                                   mean(train_num$LotFrontage, na.rm = TRUE))
train_num$MasVnrArea = replace_na(train_num$MasVnrArea,
                                  mean(train_num$MasVnrArea, na.rm = TRUE))
train_num$GarageYrBlt = replace_na(train_num$GarageYrBlt,
                                  mean(train_num$GarageYrBlt, na.rm = TRUE))
# Parse years to ages
# Years of Garage
for(i in 1:nrow(train_num)) {
  GarYearNum = max(train_num$GarageYrBlt) - train_num$GarageYrBlt
  return(GarYearNum)
}
train_num$GarageAge = GarYearNum
# Years of House
for(i in 1:nrow(train_num)) {
  YearNum = max(train_num$YearBuilt) - train_num$YearBuilt
  return(YearNum)
}
train_num$Age = YearNum
summary(train_num)
# remove the year column
train_num = train_num %>% 
  select(-YearBuilt, -GarageYrBlt)


ncol(train_num)
ncol(scaleCol)
ncol(typeCol)

write_csv(train_num, "train_num.csv")
write_csv(scaleCol, "scaleCol.csv")
write_csv(typeCol, "typeCol.csv")




## Pattern Discovery
# KMmans: `train_num`
# EFA: `scaleCol`

## Dimention Reduction
# PCA: `train_num`
# 


# Modeling: `train_num` + all scales that can be factored
#   Random Forest
#   Boosting




