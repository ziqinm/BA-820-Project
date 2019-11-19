options(stringsAsFactors=FALSE)
options(digits=3)


library(tidyverse)
library(factoextra)
library(skimr)
library("corrplot")

train_orig = read_csv("train.csv")
train = train_orig %>% 
  select(-SalePrice)
skim(train_num)
train %>% select(-textName, -scaleName)-> train_num
train_num$GarageYrBlt = replace_na(train_num$GarageYrBlt,
                                   mean(train_num$GarageYrBlt, na.rm = TRUE))
train_num$LotFrontage = replace_na(train_num$LotFrontage,
                                   mean(train_num$LotFrontage, na.rm = TRUE))
train_num$MasVnrArea = replace_na(train_num$MasVnrArea,
                                  mean(train_num$MasVnrArea, na.rm = TRUE))

train_num
trian_p=prcomp(train_num,center=TRUE,scale=TRUE)
class(trian_p)
summary(trian_p)
fviz_screeplot(trian_p,addlabels=T,ncp=15)
get_eigenvalue(trian_p)
##based on thses two methods, I would like to choose 12-pca. 







