library(data.table)
library(tidyverse)
library(stringr)
library(readxl)
library(broom)

source('~/Google Drive/Finance/R Projects/helper_functions.R')
#================================ Header ================================

# ============================= ASSESSMENT ===================================

# ============================= VISUALIZATION ===================================

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

View(out)
# for (i in seq(0, 1, .01)){
#   
#   n_cor_pos <- pred_test %>% 
#     mutate(aux = pred > i) %>% 
#     filter(actual == aux,
#            actual == 1) %>% 
#     nrow()
#   tot_pos <- pred_test %>% 
#     filter(actual == 1) %>% 
#     nrow()
#   sens <- n_cor_pos/tot_pos
#   
#   n_cor_neg <- pred_test %>% 
#     mutate(aux = pred > i) %>% 
#     filter(actual == aux,
#            actual == 0) %>% 
#     nrow()
#   tot_neg <- pred_test %>% 
#     filter(actual == 0) %>% 
#     nrow()
#   spec <- n_cor_neg/tot_neg
#   
#   aux <- data_frame(cut = i, spec = spec, sens = sens)
#   out <- bind_rows(out, aux)
#   
# }

# roc curve
p_ROC <- out %>% 
  ggplot(aes(x = (1 - spec), y = sens, color = model)) +
  geom_point() +
  geom_abline(slope = 1) +
  labs(title = "ROC")

# histogram
p_hist <- pred_test %>% 
  gather(key = model, value = pred, -actual) %>% 
  ggplot(aes(x = pred, fill = as.factor(actual))) +
  geom_histogram(alpha = 0.6, position = "identity") +
  facet_wrap(~model) + 
  labs(title = "Histogram - tfidf",
       fill = "Actual Value",
       x = "Predicted Probability",
       y = NULL)

# accuracy
out %>% 
  group_by(model) %>% 
  summarise(acc_max = max(acc))

ggsave("viz/ROC_tfidf_Yelp.jpeg", p_ROC, device = "jpeg")
ggsave("viz/hist_tfidf_Yelp.jpeg", p_hist, device = "jpeg")
