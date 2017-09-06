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
# Pacakges to save the logs
library(jsonlite)
# ============================= Prepare the Data ===================================
to_log <- list(file_name = "./data/LDA_features_AmazonBooks.rds",
               params_xg = list(
                 nrounds = 60, 
                 objective = "binary:logistic",
                 eta = 0.1,
                 subsample = 0.5,
                 colsample_bytree = 0.8,
                 seed = 1,
                 nthread = 3, 
                 max_delta_step = 10
               ),
               params_glm = list(
                 family = 'binomial', 
                 alpha = 1,
                 type.measure = "auc",
                 nfolds = 5,
                 thresh = 1e-3,
                 maxit = 1e3),
               machine_info = Sys.info()['version'],
               user = Sys.info()['user']
)

df <- as_tibble(readRDS(to_log$file_name))

label <- select(df, binary_rating)

feat <- select(df, -review_id, -rating) 
colnames(feat)[-1] <- paste0("feat", colnames(feat)[-1])

# Make features a sparse matrix for the models
feat_sparse <- Matrix::sparse.model.matrix(data = feat,
                                           object = binary_rating ~ .-1)

# ============================= Split Train and Test Sets ===================================
ind <- sample(1:nrow(label), round(nrow(label)*0.80), replace = FALSE)

train_feat <- feat_sparse[ind, ]
train_label <- label[ind, ]

test_feat <- feat_sparse[-ind, ]
test_label <- label[-ind, ]

# ============================= Train the Model ===================================
mod_glmnet  <-  cv.glmnet(x = train_feat,
                          y = train_label$binary_rating, 
                          family = to_log$params_glm$family, 
                          alpha = to_log$params_glm$alpha,
                          type.measure = to_log$params_glm$type.measure,
                          nfolds = to_log$params_glm$nfolds,
                          thresh = to_log$params_glm$thresh,
                          maxit = to_log$params_glm$maxit)

mod_xgboost <- xgboost(data = train_feat,
                       label = train_label$binary_rating,
                       nrounds = to_log$params_xg$nrounds,
                       objective = to_log$params_xg$objective,
                       eta = to_log$params_xg$eta,
                       subsample = to_log$params_xg$subsample,
                       colsample_bytree = to_log$params_xg$colsample_bytree,
                       seed = to_log$params_xg$seed,
                       nthread = to_log$params_xg$nthread,
                       max_delta_step = to_log$params_xg$max_delta_step
)

# ============================= Predict on Test Set ===================================
pred_test <- data_frame(pred_glmnet = predict(mod_glmnet, test_feat, type = 'response')[, 1],
                        pred_xgboost = predict(mod_xgboost, test_feat, type = 'response'),
                        actual = test_label$binary_rating)


source("./mod_assessment.R")

# Include accuracy and AUC in the logs
to_log$results <- list(accuracy, auc)

toJSON(to_log, pretty = TRUE) %>% 
  write(file = paste0("log/", 
                      str_replace_all(strftime(Sys.time() , "%Y-%m-%dT%H:%M:%S"), ":", ""),
                      "_",
                      str_extract(to_log$file_name, "[A-Za-z]{1,10}_[A-Za-z]{1,20}_[A-Za-z0-9]{1,10}"),
                      ".log"
  ))

cat(cat(paste0(auc$model, " got an AUC of:\n\t", auc$auc), sep = "\n"),
    cat(paste0(accuracy$model, " got an Accuracy of:\n\t", accuracy$acc_max), sep = "\n"))
