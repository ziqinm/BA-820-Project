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
textName = c("MSZoning", "Street", "LotShape", "LandContour", "Utilities",
             "LotConfig", "LandSlope", "Neighborhood", "Condition1", "Condition2",
             "BldgType", "HouseStyle", "RoofStyle", "RoofMatl", "Exterior1st",
             "Exterior2nd", "MasVnrType", "ExterQual", "ExterCond", "Foundation",
             "BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2",
             "Heating", "HeatingQC", "CentralAir", "Electrical", "KitchenQual",
             "Functional", "FireplaceQu", "GarageType", "GarageFinish", "GarageQual",
             "GarageCond", "PavedDrive", "PoolQC", "Fence", "MiscFeature",
             "SaleType", "SaleCondition", "Alley")
textCol = train %>% select(textName)
glimpse(textCol)

### Convert rank categories to scales for EFA ###

## Can be factored, then used in SML methods
# LotShape: Reg = 4, IR1 = 3, IR2 = 2, IR1 = 1
textCol$LotShape = as.numeric(factor(textCol$LotShape))
# LandDlope: Gtl = 1, Mod = 2, Sev = 3
textCol$LandSlope = as.numeric(factor(textCol$LandSlope))
# ExterQual: Ex = 1, Gd = 3, TA = 4, Fa = 2
textCol$ExterQual = as.numeric(factor(textCol$ExterQual))
# ExterCond: TA = 5, Gd = 3, Fa = 2, Po = 4, Ex = 1
textCol$ExterCond = as.numeric(factor(textCol$ExterCond))
# BsmtQual: Ex = 1, TA = 4, Gd = 3, FA = 2, NA = 0
textCol$BsmtQual = as.numeric(factor(textCol$BsmtQual))
replace_na(textCol$BsmtQual, 0)
# BsmtCond: TA = 4, Gd = 2, FA = 1, Po = 3, NA = 0
textCol$BsmtCond = as.numeric(factor(textCol$BsmtCond))
replace_na(textCol$BsmtCond, 0)
# BsmtExposure
# HeatingQC
# CentralAir
# KitchenQual
# FireplaceQu
# GarageQual
# GarageCond



## Cannot/have NA in test set(cheating this way) => EFA only
# Utilities: AllPub = 4, NoSewr = 3, NoSeWa = 2, ELO = 1
unique(textCol$Utilities)
textCol$Utilities = gsub("AllPub", 4, textCol$Utilities)
textCol$Utilities = gsub("NoSeWa", 2, textCol$Utilities)
# BsmtFinType1
# BsmtFinType2
# Electrical
# Functional
# GarageFinish
# PavedFrive
# PoolQC


           
# Extraxt other numeric scale variables
train %>% select(-textName) %>% glimpse()
scaleName = c("OverallQual", "OverallCond")
scaleCol = train %>% select(scaleName)
glimpse(scaleCol)
train %>% select(-textName, -scaleName) %>% glimpse() -> train_num




# Techniques:
#   CLustering: HClust(numeric + char), KMeans(numeric)
#   Dimentionality Reduction: PCA(train_num), EFA
#   Text analytics: `textCol`

# Modeling: `traiin_num` + all scales that can be factored
#   Linear Regression
#   Ridge/Lasso
#   Random Forest
#   Boosting



