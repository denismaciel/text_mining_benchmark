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
#================================ Header ================================ 

to_log <- list(file_path = list(technique  = technique,
                                dataset = dataset,
                                size = size),
               folder = folder,
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
               user = Sys.info()['user'],
               results = list()
)


# ============================= Load Data ===================================
read_input <- function(approach, tt, feat_or_label){

readRDS(paste0(to_log$folder,
               paste(to_log$file_path$technique, approach, to_log$file_path$dataset, 
                     tt, to_log$file_path$size, feat_or_label,
                     sep = "_"),
               ".RDS"))
}

# read_input <- function(approach, tt, feat_or_label){
#   
#   paste0(to_log$folder,
#          paste(to_log$file_path$technique, approach, to_log$file_path$dataset, 
#                tt, to_log$file_path$size, feat_or_label,
#                sep = "_"),
#          ".RDS")
# }

blind_test_featsparse <- read_input("blind", "test", "feat")
blind_train_featsparse <- read_input("blind", "train", "feat")
blind_train <- read_input("blind", "train", "label")
blind_test <- read_input("blind", "test", "label")

mix_test_featsparse <- read_input("mix", "test", "feat")
mix_train_featsparse <- read_input("mix", "train", "feat")
mix_train <- read_input("mix", "train", "label")
mix_test <- read_input("mix", "test", "label")

# ======================= Create blind_seen & blind_test ========================

ind <- sample(1:nrow(blind_train_featsparse), round(nrow(blind_train_featsparse)*0.80), replace = FALSE)

blind_seen_train_label <- blind_train[ind, ]$binary_rating
blind_seen_train_featsparse <- blind_train_featsparse[ind, ]

blind_seen_test_label <- blind_train[-ind, ]$binary_rating
blind_seen_test_featsparse <- blind_train_featsparse[-ind, ]

blind_blind_label <- blind_test$binary_rating

# Blind Blind cannot contain words that are not present in blind_seem
blind_blind_featsparse <- blind_test_featsparse

colnames <- colnames(blind_blind_featsparse)
col_ind <- colnames %in% unique(c(colnames(blind_seen_train_featsparse)))
blind_blind_featsparse <- blind_blind_featsparse[, colnames[col_ind]]

mix_train_label <- mix_train$binary_rating
mix_test_label <- mix_test$binary_rating

# ============================= Train the Model ===================================
run_models <- function(features, label){
  if(!is.vector(label)) error("label must be a vector")
  
  mod_glmnet  <-  cv.glmnet(x = features,
                            y = label,
                            family = to_log$params_glm$family,
                            alpha = to_log$params_glm$alpha,
                            type.measure = to_log$params_glm$type.measure,
                            nfolds = to_log$params_glm$nfolds,
                            thresh = to_log$params_glm$thresh,
                            maxit = to_log$params_glm$maxit)
  
  mod_xgboost <- xgboost(data = features,
                         label = label,
                         nrounds = to_log$params_xg$nrounds,
                         objective = to_log$params_xg$objective,
                         eta = to_log$params_xg$eta,
                         subsample = to_log$params_xg$subsample,
                         colsample_bytree = to_log$params_xg$colsample_bytree,
                         seed = to_log$params_xg$seed,
                         nthread = to_log$params_xg$nthread,
                         max_delta_step = to_log$params_xg$max_delta_step)
  
  return(list(mod_glmnet = mod_glmnet,
              mod_xgboost = mod_xgboost))
}

# TRAIN!!!
mod_blind_seen_train <- run_models(label = blind_seen_train_label,
                                   features = blind_seen_train_featsparse)
mod_mix <- run_models(label = mix_train_label,
                      features = mix_train_featsparse)

# ============================= Predict on Test Set ===================================
make_predicitons <- function(features, label, models){
  
  pred_test <- data_frame(
    pred_glmnet = tryCatch(
      {predict(models$mod_glmnet, features, type = 'response')[, 1]},
      error = function(e) {
        return(0)
      }
  ),
  pred_xgboost = predict(models$mod_xgboost, features, type = 'response'),
  actual = label
  )

  return(pred_test)
}

# BLIND APPROACH
predictions_mix <- make_predicitons(features = mix_test_featsparse,
                                    label = mix_test_label,
                                    models = mod_mix)

predictions_blind_seen <- make_predicitons(features = blind_seen_test_featsparse,
                                           label = blind_seen_test_label,
                                           models = mod_blind_seen_train)

predicitons_blind_blind <- make_predicitons(features = blind_blind_featsparse,
                                            label = blind_blind_label,
                                            models = mod_blind_seen_train)

# ============================= ASSESSMENT ===================================
assess_model <- function(pred_test){
  # uses pred_test as input
  out <- data_frame(cut = numeric(),
                    spec = numeric(),
                    sens = numeric(),
                    acc = numeric(),
                    model = character())
  
  tot_pos <- sum(pred_test$actual)
  tot_neg <- sum(!pred_test$actual)
  
  for(i in seq(0, 1, 0.01)){
    
    # number of corrected classified positive and negative
    n_cor_pos_glmnet <- sum((pred_test$pred_glmnet > i) & pred_test$actual)
    n_cor_neg_glmnet <- sum((pred_test$pred_glmnet < i) & !pred_test$actual)
    
    n_cor_pos_xgboost <- sum((pred_test$pred_xgboost > i) & pred_test$actual)
    n_cor_neg_xgboost <- sum((pred_test$pred_xgboost < i) & !pred_test$actual)
    
    # sensitivity and specificity
    sens_glmnet <- n_cor_pos_glmnet/tot_pos
    spec_glmnet <- n_cor_neg_glmnet/tot_neg
    
    sens_xgboost <- n_cor_pos_xgboost/tot_pos
    spec_xgboost <- n_cor_neg_xgboost/tot_neg
    
    # accuracy
    acc_glmnet <- (n_cor_pos_glmnet + n_cor_neg_glmnet)/nrow(pred_test)
    acc_xgboost <- (n_cor_pos_xgboost + n_cor_neg_xgboost)/nrow(pred_test)
    
    aux <- data_frame(cut = c(i, i),
                      spec = c(spec_glmnet, spec_xgboost),
                      sens = c(sens_glmnet, sens_xgboost),
                      acc = c(acc_glmnet, acc_xgboost),
                      model = c("glmnet", "xgboost"))
    out <- bind_rows(out, aux)
  }
  
  # accuracy
  accuracy <- out %>%
    group_by(model) %>%
    summarise(acc_max = max(acc))
  
  auc <- caTools::colAUC(pred_test[c('pred_glmnet', 'pred_xgboost')], pred_test$actual) %>%
    t() %>%
    broom::tidy() %>%
    select(model = .rownames, auc = X0.vs..1) %>%
    mutate(model = str_replace(model, "pred_", ""))
  
  # If glmnet does not work assign zeroes to its metrics
  if (sum(pred_test$pred_glmnet) == 0) {
    auc[auc$model == "glmnet", ]$auc <- 0 
    accuracy[accuracy$model == "glmnet", ]$acc_max <- 0 
  }
  
  return(list(accuracy, auc))
}

to_log$results$mix <- assess_model(predictions_mix)
to_log$results$blind_seen <- assess_model(predictions_blind_seen)
to_log$results$blind_blind <- assess_model(predicitons_blind_blind)

# ============================= Log Results ===================================
log <- toJSON(to_log, pretty = TRUE)  

file <-  paste0("log/", 
                str_replace_all(strftime(Sys.time() , "%Y-%m-%dT%H:%M:%S"), ":", ""),
                "_",
                to_log$file_path$technique,
                "_",
                to_log$file_path$dataset,
                "_",
                to_log$file_path$size,
                ".log")

write(log, file)

# Remove everything except "loop_grid" and "folder", which is used to control loop
rm(list = setdiff(ls(), c("loop_grid", "folder")))

