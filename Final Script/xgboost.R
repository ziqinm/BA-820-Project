options(stringsAsFactors = FALSE)

library(tidyverse)
library(xgboost)
library(yardstick)


# Model on past set -------------------------------------------------------

input = read_csv("Model/past_df.csv")
glimpse(input)
dim(input)
input_mat = as.matrix(input)

xgmod = xgboost(data = input_mat[, -25],
                label = input_mat[, 25],
                objective = "reg:squarederror",
                nrounds = 15)




# Validate on valid set ---------------------------------------------------

valid = read_csv("Model/valid_df.csv")
glimpse(valid)
dim(valid)

preds = predict(xgmod, as.matrix(valid[, -25]))
price_test = valid[, 25]
price_test = unlist(price_test)
rsq_vec(price_test, preds)
# 0.8668892
mean((preds - price_test)^2)
