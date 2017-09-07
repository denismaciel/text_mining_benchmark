# ============================= Header ===================================
# General Purpose Packages
library(data.table)
library(tidyverse)
library(stringr)
# Packages to prepare the data
library(Matrix)
# Packages to train the model
library(xgboost)
library(glmnet)

# ============================= Prepare the Data ===================================
df <- readRDS("features/LSA_yelp_train_10000.RDS")

label <- select(df, binary_rating)

feat <- select(df, -review_id, -rating) 



# Make features a sparse matrix for the models
feat_sparse <- Matrix::sparse.model.matrix(data = feat,
                                           object = binary_rating ~ .-1)

# ============================= Split Train and Test Sets ===================================
ind <- sample(1:NROW(label), round(NROW(label)*0.80), replace = FALSE)



train_feat <- feat_sparse[ind, ]
train_label <- label[ind, ]

test_feat <- feat_sparse[-ind, ]
test_label <- label[-ind, ]

# ============================= Train the Model ===================================
mod_glmnet  <-  cv.glmnet(x = train_feat,
                          y = train_label, 
                          family = 'binomial', 
                          alpha = 1,
                          type.measure = "auc",
                          nfolds = 5,
                          thresh = 1e-3,
                          maxit = 1e3)

mod_xgboost <- xgboost(data = train_feat,
                       label = train_label,
                       nrounds = 100,
                       objective = "binary:logistic")

# ============================= Predict on Test Set ===================================
pred_test <- data_frame(pred_glmnet = predict(mod_glmnet, test_feat, type = 'response')[, 1],
                        pred_xgboost = predict(mod_xgboost, test_feat, type = 'response'),
                        actual = test_label)







if(!require("ROCR")) install.packages("ROCR"); library("ROCR")
if(!require("pROC")) install.packages("pROC"); library("pROC")
result <- as.data.frame(pred_test)
roc_obj_xgb <- roc(result$actual,round(result$pred_xgboost, digits = 0))
roc_obj_glm <- roc(result$actual,round(result$pred_glmnet, digits = 0))
#detach(package:glmnet)
auc(roc_obj_xgb)
auc(roc_obj_glm)