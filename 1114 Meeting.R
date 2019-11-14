options(stringsAsFactors = FALSE)

# Refer:
# https://www.kaggle.com/erikbruin/house-prices-lasso-xgboost-and-a-detailed-eda
# 




# Techniques:
#   CLustering: HClust(num + char), KMeans
#   Dimentionality Reduction: PCA
#   Text analytics?

# Modeling:
#   Linear Regression
#   Ridge/Lasso
#   Random Forest
#   Boosting

# ----------------------------- #

library(tidyverse)
library(skimr)

train_orig = read_csv("train.csv")
summary(train_orig)
glimpse(train_orig)
skim(train_orig)


## Remove `SalePrice` column
train = train_orig %>% 
  select(-SalePrice)
dim(train)
## Move `Id` to rownames, then delete
rownames(train) = train$Id
train = train %>% select(-Id)

## Extract text columns 
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
train %>% select(-textName) %>% glimpse()

scaleName = c("OverallQual", "OverallCond")
scaleCol = train %>% select(scaleName)
glimpse(scaleCol)
train %>% select(-textName, -scaleName) %>% glimpse()



