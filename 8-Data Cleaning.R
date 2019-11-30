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

scaleFct = chrCol %>% 
  select(-categName)

### Convert scale-like character variables into factors ###
# LotShape: Reg = 4, IR1 = 3, IR2 = 2, IR1 = 1
scaleFct$LotShape = factor(scaleFct$LotShape)
ordered(scaleFct$LotShape, levels = c("IR1", "IR2", "IR3", "Reg"))
# LandSlope: Gtl = 1, Mod = 2, Sev = 3
scaleFct$LandSlope = factor(scaleFct$LandSlope)
ordered(scaleFct$LandSlope, levels = c("Gtl", "Mod", "Sev"))
# ExterQual: Ex = 4, Gd = 3, TA = 2, Fa = 1
scaleFct$ExterQual = factor(scaleFct$ExterQual)
ordered(scaleFct$ExterQual, levels = c("Fa", "TA", "Gd", "Ex"))
12312312312
# ExterCond: TA = 3, Gd = 4, Fa = 2, Po = 1, Ex = 5
scaleFct$ExterCond = factor(scaleFct$ExterCond)
# BsmtQual: Ex = 4, TA = 2, Gd = 3, FA = 1, NA = 0
scaleFct$BsmtQual = factor(scaleFct$BsmtQual)
scaleFct$BsmtQual = replace_na(scaleFct$BsmtQual, 0)
# BsmtCond: TA = 4, Gd = 2, FA = 1, Po = 3, NA = 0
scaleFct$BsmtCond = as.numeric(factor(scaleFct$BsmtCond))
scaleFct$BsmtCond = replace_na(scaleFct$BsmtCond, 0)
# BsmtExposure: No = 4, Mn = 3, Gd = 2, Av = 1, NA = 0
scaleFct$BsmtExposure = as.numeric(factor(scaleFct$BsmtExposure))
scaleFct$BsmtExposure = replace_na(scaleFct$BsmtExposure, 0)
# HeatingQC: Ex = 1, Gd = 3, TA = 5, Fa = 2, Po = 4
scaleFct$HeatingQC = as.numeric(factor(scaleFct$HeatingQC))
# CentralAir: Y = 2, N = 1
scaleFct$CentralAir = as.numeric(factor(scaleFct$CentralAir))
# KitchenQual:  Ex = 1, TA = 4, Gd = 3, FA = 2
scaleFct$KitchenQual = as.numeric(factor(scaleFct$KitchenQual))
# FireplaceQu: Ex = 1, Gd = 3, TA = 5, Fa = 2, Po = 4, NA = 0
scaleFct$FireplaceQu = as.numeric(factor(scaleFct$FireplaceQu))
scaleFct$FireplaceQu = replace_na(scaleFct$FireplaceQu, 0)
# GarageQual: Ex = 1, Gd = 3, TA = 5, Fa = 2, Po = 4, NA = 0
scaleFct$GarageQual = as.numeric(factor(scaleFct$GarageQual))
scaleFct$GarageQual = replace_na(scaleFct$GarageQual, 0)
# GarageCond: Ex = 1, Gd = 3, TA = 5, Fa = 2, Po = 4, NA = 0
scaleFct$GarageCond = as.numeric(factor(scaleFct$GarageCond))
scaleFct$GarageCond = replace_na(scaleFct$GarageCond, 0)
# PoolQC: Ex = 1, Fa = 2, Gd = 3, NA = 0
scaleFct$PoolQC = as.numeric(factor(scaleFct$PoolQC))
scaleFct$PoolQC = replace_na(scaleFct$PoolQC, 0)

## Manually factored:
# Utilities: AllPub = 4, NoSewr = 3, NoSeWa = 2, ELO = 1
# unique(scaleFct$Utilities)
scaleFct$Utilities = gsub("AllPub", 4, scaleFct$Utilities)
scaleFct$Utilities = gsub("NoSeWa", 2, scaleFct$Utilities)
scaleFct$Utilities = as.numeric(scaleFct$Utilities)
# BsmtFinType1: GLQ = 6, ALQ = 5, BLQ = 4, Rec = 3, LwQ = 2, Unf = 1, NA = 0
# unique(scaleFct$BsmtFinType1)
scaleFct$BsmtFinType1 = gsub("GLQ", 6, scaleFct$BsmtFinType1)
scaleFct$BsmtFinType1 = gsub("ALQ", 5, scaleFct$BsmtFinType1)
scaleFct$BsmtFinType1 = gsub("BLQ", 4, scaleFct$BsmtFinType1)
scaleFct$BsmtFinType1 = gsub("Rec", 3, scaleFct$BsmtFinType1)
scaleFct$BsmtFinType1 = gsub("LwQ", 2, scaleFct$BsmtFinType1)
scaleFct$BsmtFinType1 = gsub("Unf", 1, scaleFct$BsmtFinType1)
scaleFct$BsmtFinType1 = as.numeric(scaleFct$BsmtFinType1)
scaleFct$BsmtFinType1 = replace_na(scaleFct$BsmtFinType1, 0)
# BsmtFinType2: GLQ = 6, ALQ = 5, BLQ = 4, Rec = 3, LwQ = 2, Unf = 1, NA = 0
# unique(scaleFct$BsmtFinType2)
scaleFct$BsmtFinType2 = gsub("GLQ", 6, scaleFct$BsmtFinType2)
scaleFct$BsmtFinType2 = gsub("ALQ", 5, scaleFct$BsmtFinType2)
scaleFct$BsmtFinType2 = gsub("BLQ", 4, scaleFct$BsmtFinType2)
scaleFct$BsmtFinType2 = gsub("Rec", 3, scaleFct$BsmtFinType2)
scaleFct$BsmtFinType2 = gsub("LwQ", 2, scaleFct$BsmtFinType2)
scaleFct$BsmtFinType2 = gsub("Unf", 1, scaleFct$BsmtFinType2)
scaleFct$BsmtFinType2 = as.numeric(scaleFct$BsmtFinType2)
scaleFct$BsmtFinType2 = replace_na(scaleFct$BsmtFinType2, 0)
# Electrical: SBrkr = 5, FuseA = 4, FuseF = 3, FuseP = 2, Mix = 1, NA = 0
# unique(scaleFct$Electrical)
scaleFct$Electrical = gsub("SBrkr", 5, scaleFct$Electrical)
scaleFct$Electrical = gsub("FuseA", 4, scaleFct$Electrical)
scaleFct$Electrical = gsub("FuseF", 3, scaleFct$Electrical)
scaleFct$Electrical = gsub("FuseP", 2, scaleFct$Electrical)
scaleFct$Electrical = gsub("Mix", 1, scaleFct$Electrical)
scaleFct$Electrical = as.numeric(scaleFct$Electrical)
scaleFct$Electrical = replace_na(scaleFct$Electrical, 0)
# Functional: Typ = 8, Min1 = 7, Min2 = 6, Mod = 5, Maj1 = 4, Maj2 = 3, Sev = 2, Sal = 1
# unique(scaleFct$Functional)
scaleFct$Functional = gsub("Typ", 8, scaleFct$Functional)
scaleFct$Functional = gsub("Min1", 7, scaleFct$Functional)
scaleFct$Functional = gsub("Min2", 6, scaleFct$Functional)
scaleFct$Functional = gsub("Mod", 5, scaleFct$Functional)
scaleFct$Functional = gsub("Maj1", 4, scaleFct$Functional)
scaleFct$Functional = gsub("Maj2", 3, scaleFct$Functional)
scaleFct$Functional = gsub("Sev", 2, scaleFct$Functional)
scaleFct$Functional = gsub("Sal", 1, scaleFct$Functional)
scaleFct$Functional = as.numeric(scaleFct$Functional)
# GarageFinish: RFn = 3, Fin = 2, Unf = 1, NA = 0
# unique(scaleFct$GarageFinish)
scaleFct$GarageFinish = gsub("RFn", 3, scaleFct$GarageFinish)
scaleFct$GarageFinish = gsub("Fin", 2, scaleFct$GarageFinish)
scaleFct$GarageFinish = gsub("Unf", 1, scaleFct$GarageFinish)
scaleFct$GarageFinish = as.numeric(scaleFct$GarageFinish)
scaleFct$GarageFinish = replace_na(scaleFct$GarageFinish, 0)
# PavedDrive: Y = 3, P = 2, N = 1
# unique(scaleFct$PavedDrive)
scaleFct$PavedDrive = gsub("Y", 3, scaleFct$PavedDrive)
scaleFct$PavedDrive = gsub("P", 2, scaleFct$PavedDrive)
scaleFct$PavedDrive = gsub("N", 1, scaleFct$PavedDrive)
scaleFct$PavedDrive = as.numeric(scaleFct$PavedDrive)



           
# Extraxt other numeric scale variables
train %>% select(-chrName) %>% glimpse()
scaleName = c("OverallQual", "OverallCond")
scaleNum = train %>% select(scaleName)
scaleFct = cbind(scaleFct, scaleNum)
glimpse(scaleFct)
skim(scaleFct)
which(is.na(scaleFct), arr.ind=TRUE)   
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
ncol(scaleFct)
ncol(typeCol)

write_csv(train_num, "train_dbl.csv")   # <dbl>
write_csv(scaleFct, "scale_fct.csv")     # <fct>
write_csv(typeCol, "type_chr.csv")       # <chr>




## Pattern Discovery
# PAM: use all variables to cluster
# EFA: `scaleFct`, then convert to numeric

## Dimention Reduction
# PCA: `train_num`
# 


# Modeling: `train_num` + all scales that can be factored
#   Random Forest
#   Boosting




