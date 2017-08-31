library(data.table)
library(tidyverse)
library(stringr)
library(readxl)
library(broom)
              
source('~/Google Drive/Finance/R Projects/helper_functions.R')
#================================ Header ================================
library(Matrix)
library(xgboost)
library(glmnet)

df <- readRDS("data/LDA_features_twitter_50K.rds") %>% tbl_df()
feat <- df %>% select(-review_id, -rating) 
colnames(feat)[-1] <- paste0("feat", colnames(feat)[-1])
label <- select(df, binary_rating)
feat_sparse <- Matrix::sparse.model.matrix(data = feat,
                                           object = binary_rating ~ .-1)

# Create train and test
ind <- sample(1:nrow(label), round(nrow(label)*0.80), replace = FALSE)

train_feat <- feat_sparse[ind, ]
train_label <- label[ind, ]

test_feat <- feat_sparse[-ind, ]
test_label <- label[-ind, ]


# model

mod_glmnet  <-  cv.glmnet(x = train_feat,
                          y = train_label$binary_rating, 
                          family = 'binomial', 
                          alpha = 1,
                          type.measure = "auc",
                          nfolds = 5,
                          thresh = 1e-3,
                          maxit = 1e3)

mod_xgboost <- xgboost(data = train_feat,
                       label = train_label$binary_rating,
                       nrounds = 100,
                       objective = "binary:logistic")

# ============================= TEST THE MODEL ===================================
pred_test <- data_frame(pred_glmnet = predict(mod_glmnet, test_feat, type = 'response')[, 1],
                        pred_xgboost = predict(mod_xgboost, test_feat, type = 'response'),
                        actual = test_label$binary_rating)


View(pred_test)
