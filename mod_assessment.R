#================================ Header ================================
library(data.table)
library(tidyverse)
library(stringr)
if(!require("broom")) install.packages("broom"); library(broom)
# ============================= ASSESSMENT ===================================
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
