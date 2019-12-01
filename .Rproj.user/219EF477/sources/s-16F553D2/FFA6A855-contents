options(stringsAsFactors = FALSE)

# Refer:
# https://www.kaggle.com/erikbruin/house-prices-lasso-xgboost-and-a-detailed-eda


# ----------------------------- #

library(tidyverse)
library(skimr)

train_orig = read_csv("Original Data/train.csv")
summary(train_orig)
glimpse(train_orig)
skim(train_orig)

test_orig = read_csv("Original Data/test.csv")

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
             "SaleType", "SaleCondition", "Alley", "MoSold")
chrCol = train %>% select(chrName)
glimpse(chrCol)

categName = c("MSZoning", "Street", "LandContour", "LotConfig", "Neighborhood", "Condition1",
             "Condition2", "BldgType", "HouseStyle", "RoofStyle", "RoofMatl", "Exterior1st",
             "Exterior2nd", "MasVnrType", "Foundation", "Heating", "GarageType",
             "SaleType", "SaleCondition", "Alley", "Fence", "MiscFeature", "MoSold")

typeCol = chrCol %>% 
  select(categName)

scaleFct = chrCol %>% 
  select(-categName)

### Convert scale-like character variables into factors ###
# LotShape: Reg = 4, IR1 = 3, IR2 = 2, IR1 = 1
scaleFct$LotShape = factor(scaleFct$LotShape)
fct_explicit_na(scaleFct$LotShape, "NA")
ordered(scaleFct$LotShape, levels = c("IR1", "IR2", "IR3", "Reg"))
# LandSlope: Gtl = 1, Mod = 2, Sev = 3
scaleFct$LandSlope = factor(scaleFct$LandSlope)
fct_explicit_na(scaleFct$LandSlope, "NA")
ordered(scaleFct$LandSlope, levels = c("Gtl", "Mod", "Sev"))
# ExterQual: Ex = 5, Gd = 4, TA = 3, Fa = 2, Po = 1
scaleFct$ExterQual = factor(scaleFct$ExterQual)
fct_explicit_na(scaleFct$ExterQual, "NA")
ordered(scaleFct$ExterQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
# ExterCond: TA = 3, Gd = 4, Fa = 2, Po = 1, Ex = 5
scaleFct$ExterCond = factor(scaleFct$ExterCond)
fct_explicit_na(scaleFct$ExterCond, "NA")
ordered(scaleFct$ExterCond, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
# BsmtQual: Ex = 5, TA = 3, Gd = 4, FA = 2, Po = 1, NA = 0
scaleFct$BsmtQual = factor(scaleFct$BsmtQual)
fct_explicit_na(scaleFct$BsmtQual, "NA")
ordered(scaleFct$BsmtQual, levels = c("NA", "Po", "Fa", "TA", "Gd", "Ex"))
# BsmtCond: Ex = 5, TA = 3, Gd = 4, FA = 2, Po = 1, NA = 0
scaleFct$BsmtCond = factor(scaleFct$BsmtCond)
fct_explicit_na(scaleFct$BsmtCond, "NA")
ordered(scaleFct$BsmtCond, levels = c("NA", "Po", "Fa", "TA", "Gd", "Ex"))
# BsmtExposure: No = 1, Mn = 2, Gd = 4, Av = 3, NA = 0
scaleFct$BsmtExposure = factor(scaleFct$BsmtExposure)
fct_explicit_na(scaleFct$BsmtExposure, "NA")
ordered(scaleFct$BsmtExposure, levels = c("NA", "No", "Mn", "Av", "Gd"))
# HeatingQC: TA = 3, Gd = 4, Fa = 2, Po = 1, Ex = 5
scaleFct$HeatingQC = factor(scaleFct$HeatingQC)
fct_explicit_na(scaleFct$HeatingQC, "NA")
ordered(scaleFct$HeatingQC, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
# CentralAir: Y = 2, N = 1
scaleFct$CentralAir = factor(scaleFct$CentralAir)
fct_explicit_na(scaleFct$CentralAir, "NA")
ordered(scaleFct$CentralAir, levels = c("N", "Y"))
# KitchenQual:   Ex = 5, Gd = 4, TA = 3, Fa = 2, Po = 1
scaleFct$KitchenQual = factor(scaleFct$KitchenQual)
fct_explicit_na(scaleFct$KitchenQual, "NA")
ordered(scaleFct$KitchenQual, levels = c("Po", "Fa", "Ta", "Gd", "Ex"))
# FireplaceQu: Ex = 5, Gd = 4, TA = 3, Fa = 2, Po = 1, NA = 0
scaleFct$FireplaceQu = factor(scaleFct$FireplaceQu)
fct_explicit_na(scaleFct$FireplaceQu, "NA")
ordered(scaleFct$FireplaceQu, levels = c("NA", "Po", "Fa", "Ta", "Gd", "Ex"))
# GarageQual: Ex = 5, Gd = 4, TA = 3, Fa = 2, Po = 1, NA = 0
scaleFct$GarageQual = factor(scaleFct$GarageQual)
fct_explicit_na(scaleFct$GarageQual, "NA")
ordered(scaleFct$GarageQual, levels = c("NA", "Po", "Fa", "Ta", "Gd", "Ex"))
# GarageCond: Ex = 5, Gd = 4, TA = 3, Fa = 2, Po = 1, NA = 0
scaleFct$GarageCond = factor(scaleFct$GarageCond)
fct_explicit_na(scaleFct$GarageCond, "NA")
ordered(scaleFct$GarageCond, levels = c("NA", "Po", "Fa", "Ta", "Gd", "Ex"))
# PoolQC: Ex = 4, Fa = 1, Gd = 3, NA = 0, TA = 2
scaleFct$PoolQC = factor(scaleFct$PoolQC)
fct_explicit_na(scaleFct$PoolQC, "NA")
ordered(scaleFct$PoolQC, levels = c("NA", "Fa", "TA","Gd", "Ex"))
# Utilities: AllPub = 4, NoSewr = 3, NoSeWa = 2, ELO = 1
scaleFct$Utilities = factor(scaleFct$Utilities)
fct_explicit_na(scaleFct$Utilities, "NA")
ordered(scaleFct$Utilities, levels = c("ELO", "NoSeWa", "NoSewr", "AllPub"))
# BsmtFinType1: GLQ = 6, ALQ = 5, BLQ = 4, Rec = 3, LwQ = 2, Unf = 1, NA = 0
scaleFct$BsmtFinType1 = factor(scaleFct$BsmtFinType1)
fct_explicit_na(scaleFct$BsmtFinType1, "NA")
ordered(scaleFct$BsmtFinType1, levels = c("NA", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"))
# BsmtFinType2: GLQ = 6, ALQ = 5, BLQ = 4, Rec = 3, LwQ = 2, Unf = 1, NA = 0
scaleFct$BsmtFinType2 = factor(scaleFct$BsmtFinType2)
fct_explicit_na(scaleFct$BsmtFinType2, "NA")
ordered(scaleFct$BsmtFinType2, levels = c("NA", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"))
# Electrical: SBrkr = 5, FuseA = 4, FuseF = 3, FuseP = 2, Mix = 1, NA = 0
scaleFct$Electrical = factor(scaleFct$Electrical)
fct_explicit_na(scaleFct$Electrical, "NA")
ordered(scaleFct$Electrical, levels = c("NA", "Mix", "FuseP", "FuseF", "FuseA", "SBrkr"))
# Functional: Typ = 8, Min1 = 7, Min2 = 6, Mod = 5, Maj1 = 4, Maj2 = 3, Sev = 2, Sal = 1
scaleFct$Functional = factor(scaleFct$Functional)
fct_explicit_na(scaleFct$Functional, "NA")
ordered(scaleFct$Functional, levels = c("Sal", "Sev", "Maj2", "Maj1", "Mod", "Min2", "Min1", "Typ"))
# GarageFinish: RFn = 3, Fin = 2, Unf = 1, NA = 0
scaleFct$GarageFinish = factor(scaleFct$GarageFinish)
fct_explicit_na(scaleFct$GarageFinish, "NA")
ordered(scaleFct$GarageFinish, levels = c("NA", "Unf", "Fin", "RFn"))
# PavedDrive: Y = 3, P = 2, N = 1
scaleFct$PavedDrive = factor(scaleFct$PavedDrive)
fct_explicit_na(scaleFct$PavedDrive, "NA")
ordered(scaleFct$PavedDrive, levels = c("NA", "N", "P", "Y", "Mod"))



           
# Extraxt other numeric scale variables
train %>% select(-chrName) %>% glimpse()
scaleName = c("OverallQual", "OverallCond")
scaleNum = train %>% select(scaleName)
scaleNum$OverallQual = factor(scaleNum$OverallQual)
fct_explicit_na(scaleNum$OverallQual, "0")
ordered(scaleNum$OverallQual, levels = c("0", "1", "2", "3", "4", "5",
                                         "6", "7", "8", "9", "10"))
scaleNum$OverallCond = factor(scaleNum$OverallCond)
fct_explicit_na(scaleNum$OverallCond, "0")
ordered(scaleNum$OverallCond, levels = c("0", "1", "2", "3", "4", "5",
                                         "6", "7", "8", "9", "10"))


scaleFct = cbind(scaleFct, scaleNum)
glimpse(scaleFct)
skim(scaleFct)
# which(is.na(scaleFct), arr.ind=TRUE)   
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
# Convert months to quarters
quart = function(x) {
  if (x %in% c(1, 2, 3)) {
    Quarter = "I"
  } else if (x %in% c(4, 5, 6)){
    Quarter = "II"
  } else if (x %in% c(7, 8, 9)) {
    Quarter = "III"
  } else {
    Quarter = "IV"
  }
  return(Quarter)
}
typeCol$Quarter = map_chr(typeCol$MoSold, quart)
typeCol$Quarter
typeCol = typeCol %>% select(-MoSold)
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
# Age of Garage when sold
train_num = train_num %>% 
  mutate(GarageAge = train_num$YrSold - train_num$GarageYrBlt)

# Age of House when sold
train_num = train_num %>% 
  mutate(Age = train_num$YrSold - train_num$YearBuilt)

# Age of house after reconstruction (if applicable)
train_num = train_num %>% 
  mutate(RemodAge = train_num$YrSold - train_num$YearRemodAdd)
  
summary(train_num)
# remove the year column
train_num = train_num %>% 
  select(-YearBuilt, -GarageYrBlt, -YearRemodAdd)


ncol(train_num)
ncol(scaleFct)
ncol(typeCol)

write_csv(train_num, "train_dbl.csv")    # <dbl>
write_csv(scaleFct, "scale_fct.csv")     # <fct>
write_csv(typeCol, "type_chr.csv")       # <chr>



## Pattern Discovery
# Kmeans
# EFA: `scaleFct`, then convert to numeric

## Dimention Reduction
# PCA: `train_num`
# PAM: use all variables to cluster



# Modeling: `train_num` + all scales that can be factored
#   Random Forest
#   Boosting




