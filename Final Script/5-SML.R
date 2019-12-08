options(stringsAsFactors = FALSE)


# LASSO -------------------------------------------------------------------

library(tidyverse)
library(glmnet)
library(fastDummies)


# Model on past set 
input = read_csv("Model/past_df.csv")
glimpse(input)
input_d = dummy_cols(input, select_columns = "clus") %>% select(-clus)
glimpse(input_d)

x_data_train <- model.matrix( ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC8+PC9+PC10+
                                PC11+PC12+PC13+PC14+PC15+PC16+PC17+PC18+PC19+PC20+PC21+PC22+clus_1+clus_2, input_d)

y_data_train <- input$price

fit_lasso <- cv.glmnet(x_data_train, y_data_train, alpha=1, nfolds = 6)

yhat_train_lasso <- predict(fit_lasso, x_data_train, s=fit_lasso$lambda.min)                                                  
mse_train_lasso <- mean((y_data_train - yhat_train_lasso)^2) 
mse_train_lasso

# Validate on valid set 

valid = read_csv("Model/valid_df.csv")
glimpse(valid)
valid_d = dummy_cols(valid, select_columns = "clus") %>% select(-clus)
dim(valid)

x_data_test <- model.matrix( ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC8+PC9+PC10+
                               PC11+PC12+PC13+PC14+PC15+PC16+PC17+PC18+PC19+PC20+PC21+PC22+clus_1+clus_2, valid_d)

y_data_test <- valid$price

yhat_test_lasso <- predict(fit_lasso, x_data_test, s=fit_lasso$lambda.min)                                                  
mse_test_lasso <- mean((y_data_test - yhat_test_lasso)^2)          
mse_test_lasso  

coef(fit_lasso)


fviz_contrib(train_p, choice = "var")


# Random Forest -----------------------------------------------------------

library(tidyverse)
library(magrittr)
library(ggthemes)
library(glmnet)
library(readr)
library(stringr)
library(randomForest)
library(gbm)


# Model on past set 

input = read_csv("Model/past_df.csv")
glimpse(input)
input_d = dummy_cols(input, select_columns = "clus") %>% select(-clus)
dim(input)

lf <- "price ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15+PC16+
PC17+PC18+PC19+PC20+PC21+PC22+clus_1+clus_2"
f <- as.formula(lf)
x_train <- model.matrix(f,input)[ ,-1]
y_train <- input$price


fit_rf <- randomForest(f,
                       input,
                       ntree=500,
                       do.trace=F)
varImpPlot(fit_rf)
yhat_train_rf <- predict(fit_rf,input)
mse_rf_train <- mean((yhat_train_rf-y_train)^2)
mse_rf_train

# Validate on valid set 

valid = read_csv("Model/valid_df.csv")
glimpse(input)
dim(input)

x_test <- model.matrix(f,valid)[ , -1]
y_test <- valid$price

yhat_test_rf <- predict(fit_rf,valid)
mse_rf_test <- mean((yhat_test_rf-y_test)^2)
mse_rf_test


# XGBoost -----------------------------------------------------------------

library(tidyverse)
library(xgboost)
library(yardstick)


# Model on past set 

input = read_csv("Model/past_df.csv")
glimpse(input)
input_d = dummy_cols(input, select_columns = "clus") %>% select(-clus)
input_d = input_d %>% select(PC1:PC22, clus_1, clus_2)
dim(input)
input_mat = as.matrix(input_d)
price_train = input$price

xgmod = xgboost(data = input_mat[, -25],
                label = price_train,
                objective = "reg:squarederror",
                nrounds = 15)

save(xgmod, file = "Model/xgmod.rda")



# Validate on valid set 

valid = read_csv("Model/valid_df.csv")
glimpse(valid)
valid_d = dummy_cols(valid, select_columns = "clus") %>% select(-clus)
valid_d = valid_d %>% select(PC1:PC22, clus_1, clus_2)
dim(valid_d)

preds = predict(xgmod, as.matrix(valid_d))
price_test = valid$price
price_test = unlist(price_test)
rsq_vec(price_test, preds)
# 0.8759636
mean((preds - price_test)^2)
# 854295790

