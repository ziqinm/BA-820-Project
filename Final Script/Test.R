options(stringsAsFactors = FALSE)

# Data Cleaning -----------------------------------------------------------


library(tidyverse)
library(skimr)

test_orig = read_csv("Original Data/test.csv")
rownames(test_orig) = test_orig$Id
test = test_orig %>% select(-Id)
dim(test)

chrName = c("MSZoning", "Street", "LotShape", "LandContour", "Utilities",
            "LotConfig", "LandSlope", "Neighborhood", "Condition1", "Condition2",
            "BldgType", "HouseStyle", "RoofStyle", "RoofMatl", "Exterior1st",
            "Exterior2nd", "MasVnrType", "ExterQual", "ExterCond", "Foundation",
            "BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2",
            "Heating", "HeatingQC", "CentralAir", "Electrical", "KitchenQual",
            "Functional", "FireplaceQu", "GarageType", "GarageFinish", "GarageQual",
            "GarageCond", "PavedDrive", "PoolQC", "Fence", "MiscFeature",
            "SaleType", "SaleCondition", "Alley", "MoSold")
chrCol_t = test %>% select(chrName)
glimpse(chrCol_t)

categName = c("MSZoning", "Street", "LandContour", "LotConfig", "Neighborhood", "Condition1",
              "Condition2", "BldgType", "HouseStyle", "RoofStyle", "RoofMatl", "Exterior1st",
              "Exterior2nd", "MasVnrType", "Foundation", "Heating", "GarageType",
              "SaleType", "SaleCondition", "Alley", "Fence", "MiscFeature", "MoSold")

typeCol_t = chrCol_t %>% 
  select(categName)

scaleFct_t = chrCol_t %>% 
  select(-categName)

### Convert scale-like character variables into factors ###
# LotShape: Reg = 4, IR1 = 3, IR2 = 2, IR1 = 1
scaleFct_t$LotShape = factor(scaleFct_t$LotShape)
fct_explicit_na(scaleFct_t$LotShape, "NA")
ordered(scaleFct_t$LotShape, levels = c("IR1", "IR2", "IR3", "Reg"))
# LandSlope: Gtl = 1, Mod = 2, Sev = 3
scaleFct_t$LandSlope = factor(scaleFct_t$LandSlope)
fct_explicit_na(scaleFct_t$LandSlope, "NA")
ordered(scaleFct_t$LandSlope, levels = c("Gtl", "Mod", "Sev"))
# ExterQual: Ex = 5, Gd = 4, TA = 3, Fa = 2, Po = 1
scaleFct_t$ExterQual = factor(scaleFct_t$ExterQual)
fct_explicit_na(scaleFct_t$ExterQual, "NA")
ordered(scaleFct_t$ExterQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
# ExterCond: TA = 3, Gd = 4, Fa = 2, Po = 1, Ex = 5
scaleFct_t$ExterCond = factor(scaleFct_t$ExterCond)
fct_explicit_na(scaleFct_t$ExterCond, "NA")
ordered(scaleFct_t$ExterCond, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
# BsmtQual: Ex = 5, TA = 3, Gd = 4, FA = 2, Po = 1, NA = 0
scaleFct_t$BsmtQual = factor(scaleFct_t$BsmtQual)
fct_explicit_na(scaleFct_t$BsmtQual, "NA")
ordered(scaleFct_t$BsmtQual, levels = c("NA", "Po", "Fa", "TA", "Gd", "Ex"))
# BsmtCond: Ex = 5, TA = 3, Gd = 4, FA = 2, Po = 1, NA = 0
scaleFct_t$BsmtCond = factor(scaleFct_t$BsmtCond)
fct_explicit_na(scaleFct_t$BsmtCond, "NA")
ordered(scaleFct_t$BsmtCond, levels = c("NA", "Po", "Fa", "TA", "Gd", "Ex"))
# BsmtExposure: No = 1, Mn = 2, Gd = 4, Av = 3, NA = 0
scaleFct_t$BsmtExposure = factor(scaleFct_t$BsmtExposure)
fct_explicit_na(scaleFct_t$BsmtExposure, "NA")
ordered(scaleFct_t$BsmtExposure, levels = c("NA", "No", "Mn", "Av", "Gd"))
# HeatingQC: TA = 3, Gd = 4, Fa = 2, Po = 1, Ex = 5
scaleFct_t$HeatingQC = factor(scaleFct_t$HeatingQC)
fct_explicit_na(scaleFct_t$HeatingQC, "NA")
ordered(scaleFct_t$HeatingQC, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
# CentralAir: Y = 2, N = 1
scaleFct_t$CentralAir = factor(scaleFct_t$CentralAir)
fct_explicit_na(scaleFct_t$CentralAir, "NA")
ordered(scaleFct_t$CentralAir, levels = c("N", "Y"))
# KitchenQual:   Ex = 5, Gd = 4, TA = 3, Fa = 2, Po = 1
scaleFct_t$KitchenQual = factor(scaleFct_t$KitchenQual)
fct_explicit_na(scaleFct_t$KitchenQual, "NA")
ordered(scaleFct_t$KitchenQual, levels = c("Po", "Fa", "Ta", "Gd", "Ex"))
# FireplaceQu: Ex = 5, Gd = 4, TA = 3, Fa = 2, Po = 1, NA = 0
scaleFct_t$FireplaceQu = factor(scaleFct_t$FireplaceQu)
fct_explicit_na(scaleFct_t$FireplaceQu, "NA")
ordered(scaleFct_t$FireplaceQu, levels = c("NA", "Po", "Fa", "Ta", "Gd", "Ex"))
# GarageQual: Ex = 5, Gd = 4, TA = 3, Fa = 2, Po = 1, NA = 0
scaleFct_t$GarageQual = factor(scaleFct_t$GarageQual)
fct_explicit_na(scaleFct_t$GarageQual, "NA")
ordered(scaleFct_t$GarageQual, levels = c("NA", "Po", "Fa", "Ta", "Gd", "Ex"))
# GarageCond: Ex = 5, Gd = 4, TA = 3, Fa = 2, Po = 1, NA = 0
scaleFct_t$GarageCond = factor(scaleFct_t$GarageCond)
fct_explicit_na(scaleFct_t$GarageCond, "NA")
ordered(scaleFct_t$GarageCond, levels = c("NA", "Po", "Fa", "Ta", "Gd", "Ex"))
# PoolQC: Ex = 4, Fa = 1, Gd = 3, NA = 0, TA = 2
scaleFct_t$PoolQC = factor(scaleFct_t$PoolQC)
fct_explicit_na(scaleFct_t$PoolQC, "NA")
ordered(scaleFct_t$PoolQC, levels = c("NA", "Fa", "TA","Gd", "Ex"))
# Utilities: AllPub = 4, NoSewr = 3, NoSeWa = 2, ELO = 1
scaleFct_t$Utilities = factor(scaleFct_t$Utilities)
fct_explicit_na(scaleFct_t$Utilities, "NA")
ordered(scaleFct_t$Utilities, levels = c("ELO", "NoSeWa", "NoSewr", "AllPub"))
# BsmtFinType1: GLQ = 6, ALQ = 5, BLQ = 4, Rec = 3, LwQ = 2, Unf = 1, NA = 0
scaleFct_t$BsmtFinType1 = factor(scaleFct_t$BsmtFinType1)
fct_explicit_na(scaleFct_t$BsmtFinType1, "NA")
ordered(scaleFct_t$BsmtFinType1, levels = c("NA", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"))
# BsmtFinType2: GLQ = 6, ALQ = 5, BLQ = 4, Rec = 3, LwQ = 2, Unf = 1, NA = 0
scaleFct_t$BsmtFinType2 = factor(scaleFct_t$BsmtFinType2)
fct_explicit_na(scaleFct_t$BsmtFinType2, "NA")
ordered(scaleFct_t$BsmtFinType2, levels = c("NA", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"))
# Electrical: SBrkr = 5, FuseA = 4, FuseF = 3, FuseP = 2, Mix = 1, NA = 0
scaleFct_t$Electrical = factor(scaleFct_t$Electrical)
fct_explicit_na(scaleFct_t$Electrical, "NA")
ordered(scaleFct_t$Electrical, levels = c("NA", "Mix", "FuseP", "FuseF", "FuseA", "SBrkr"))
# Functional: Typ = 8, Min1 = 7, Min2 = 6, Mod = 5, Maj1 = 4, Maj2 = 3, Sev = 2, Sal = 1
scaleFct_t$Functional = factor(scaleFct_t$Functional)
fct_explicit_na(scaleFct_t$Functional, "NA")
ordered(scaleFct_t$Functional, levels = c("Sal", "Sev", "Maj2", "Maj1", "Mod", "Min2", "Min1", "Typ"))
# GarageFinish: RFn = 3, Fin = 2, Unf = 1, NA = 0
scaleFct_t$GarageFinish = factor(scaleFct_t$GarageFinish)
fct_explicit_na(scaleFct_t$GarageFinish, "NA")
ordered(scaleFct_t$GarageFinish, levels = c("NA", "Unf", "Fin", "RFn"))
# PavedDrive: Y = 3, P = 2, N = 1
scaleFct_t$PavedDrive = factor(scaleFct_t$PavedDrive)
fct_explicit_na(scaleFct_t$PavedDrive, "NA")
ordered(scaleFct_t$PavedDrive, levels = c("NA", "N", "P", "Y", "Mod"))




# Extraxt other numeric scale variables
test %>% select(-chrName) %>% glimpse()
scaleName = c("OverallQual", "OverallCond")
scaleNum_t = test %>% select(scaleName)
scaleNum_t$OverallQual = factor(scaleNum_t$OverallQual)
fct_explicit_na(scaleNum_t$OverallQual, "0")
ordered(scaleNum_t$OverallQual, levels = c("0", "1", "2", "3", "4", "5",
                                         "6", "7", "8", "9", "10"))
scaleNum_t$OverallCond = factor(scaleNum_t$OverallCond)
fct_explicit_na(scaleNum_t$OverallCond, "0")
ordered(scaleNum_t$OverallCond, levels = c("0", "1", "2", "3", "4", "5",
                                         "6", "7", "8", "9", "10"))


scaleFct_t = cbind(scaleFct_t, scaleNum_t)
glimpse(scaleFct_t)
skim(scaleFct_t)
test %>% select(-chrName, -scaleName) %>% glimpse() -> test_num


### Missing Values ###
skim(typeCol_t)
# Replace NAs with "N/A" as a new type
typeCol_t[is.na(typeCol_t)] = "N/A"
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
typeCol_t$Quarter = map_chr(typeCol_t$MoSold, quart)
typeCol_t$Quarter
typeCol_t = typeCol_t %>% select(-MoSold)
skim(typeCol_t)


# Numeric variables: Replace with mean
skim(test_num)
summary(test_num)
# Replace NA with colmean
test_num = test_num %>% 
  mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) 

# Parse years to ages
# Age of Garage when sold
test_num = test_num %>% 
  mutate(GarageAge = test_num$YrSold - test_num$GarageYrBlt)

# Age of House when sold
test_num = test_num %>% 
  mutate(Age = test_num$YrSold - test_num$YearBuilt)

# Age of house after reconstruction (if applicable)
test_num = test_num %>% 
  mutate(RemodAge = test_num$YrSold - test_num$YearRemodAdd)

summary(test_num)
# remove the year column
test_num = test_num %>% 
  select(-YearBuilt, -GarageYrBlt, -YearRemodAdd)


ncol(test_num)
ncol(scaleFct_t)
ncol(typeCol_t)

# write_csv(test_num, "Test/test_dbl.csv")           # <dbl>
# write_csv(scaleFct_t, "Test/scale_fct_test.csv")     # <fct>
# write_csv(typeCol_t, "Test/type_chr_test.csv")       # <chr>

#----------------------------------------#



# PCA ---------------------------------------------------------------------

library(factoextra)
library(skimr)
library(gridExtra)

test_num = read.csv("Test/test_dbl.csv")
summary(test_num)
test_num_s = scale(test_num)

test_pca = predict(train_p, newdata = test_num_s)
test_pca = as.data.frame(test_pca)
test_pca = test_pca[, 1:22]
nrow(test_pca) == nrow(test_num)


# Assign Clusters ---------------------------------------------------------

library(cluster)
library(fpc)
library(gower)

glimpse(typeCol_t)
typeFct_t = typeCol_t %>% mutate_if(is.character, as.factor)
glimpse(scaleFct_t)
glimpse(test_num)

new_test = cbind(typeFct_t, scaleFct_t, test_num)
dim(new_test)
glimpse(new_test)


# Look at the centers of PAM model
summary(pam2)
index = pam2$id.med
# Extract corresponsing rows from `new_train`
center = new_train[index, ]
rownames(center) = c("1", "2")
dim(center)
# Calculate gower distance between test set and centers
gower_df = rbind(new_test, center)
test_dist = daisy(gower_df, metric = c("gower"))
test_mat = as.matrix(test_dist)
# Compare the dissimilarity values from centers
# assign closer centers(clusters) to test observations
clus_t = c()
for(i in 1:1459) {
  min = min(test_mat[1460, i], test_mat[1461, i])
  clus_t[i] = ifelse(min == test_mat[1460, i], 1, 2)
}
clus_t

# Combine clusters to test_pca
test_df = cbind(test_pca, clus_t)
dim(test_df)


# Run Model ---------------------------------------------------------------


