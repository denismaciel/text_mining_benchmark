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

# ============= Determine the model parameters and features to be used  =====================
# Every time the model is run, the results are logged to a text file for future reference.

to_log <- list(file_path = "features/",
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

# ============================= Prepare the Data ===================================
df <- as_tibble(readRDS(to_log$file_path))
if(!"binary_rating" %in% colnames(df)) stop("binary_rating is a necessary column but couldn't be found in the dataframe")

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
to_log$results_test <- list(accuracy, auc)

# ============================= Predict on Blind Set ===================================
# From the file path of the train dataset, construct file path to the blind dataset
beg <- str_locate(to_log$file_path, "features/")[, 2]
end <- str_locate(to_log$file_path, "_train")[,1]
stem <- str_sub(to_log$file_path, beg + 1 , end - 1)
to_log$blind_path <- paste0("features/", stem, "_test_5000.RDS")

blind <- readRDS(to_log$blind_path)

if(!exists("blind")) stop(paste0("Error: path to blind dataset is invalid.",
                                 "It could not be parsed from file_path"))
                          
blind <- select(df, -review_id, -rating) 
colnames(blind)[-1] <- paste0("feat", colnames(blind)[-1])

blind_sparse <- Matrix::sparse.model.matrix(data = blind,
                                            object = binary_rating ~ .-1)

pred_blind <- data_frame(pred_glmnet = predict(mod_glmnet, blind_sparse, type = 'response')[, 1],
                         pred_xgboost = predict(mod_xgboost, blind_sparse, type = 'response'),
                         actual = blind$binary_rating)

pred_test <- pred_blind
source("./mod_assessment.R")
to_log$results_blind <- list(accuracy, auc)

# ============================= Write Results to Log File ===================================
toJSON(to_log, pretty = TRUE) %>% 
  write(file = paste0("log/", 
                      str_replace_all(strftime(Sys.time() , "%Y-%m-%dT%H:%M:%S"), ":", ""),
                      "_",
                      str_extract(to_log$file_path, "[A-Za-z0-9]{1,10}_[A-Za-z]{1,20}_[A-Za-z0-9]{1,10}"),
                      ".log"
  ))

# ============================= Print Results to Screen ===================================
cat(cat(paste0("TEST DATASET: ",to_log$results_test[[1]]$model, " got an Accuracy of:\n\t", to_log$results_test[[1]]$acc_max), sep = "\n"),
    cat(paste0("TEST DATASET: ", to_log$results_test[[2]]$model, " got an AUC of:\n\t", to_log$results_test[[2]]$auc), sep = "\n"))

cat(cat(paste0("BLIND DATASET: ",to_log$results_blind[[1]]$model, " got an Accuracy of:\n\t", to_log$results_blind[[1]]$acc_max), sep = "\n"),
    cat(paste0("BLIND DATASET: ", to_log$results_blind[[2]]$model, " got an AUC of:\n\t", to_log$results_blind[[2]]$auc), sep = "\n"))

# Clear workspace
rm(list = ls())