library(data.table)
library(tidyverse)
library(stringr)
library(readxl)
library(broom)

source('~/Google Drive/Finance/R Projects/helper_functions.R')
#================================ Header ================================

library(tidytext)
library(xgboost)
library(glmnet)
library(tm)
library(Matrix)

df <- readRDS("data/yelp_reviews.rds")

# ============================= DATA MUNGING ===================================
df_tidy <- df %>% 
  # select(review_id, review_text) %>% 
  unnest_tokens(word, review_text) %>% 
  filter(!word %in% stop_words$word) %>% 
  count(review_id, word, sort = TRUE) %>% 
  group_by(review_id) %>% 
  mutate(rank = row_number(),
         total = sum(n),
         `term frequency` = n/total)

df_tfidf <- df_tidy %>%
  bind_tf_idf(word, review_id, n)

feat <- df_tfidf %>% 
  cast_sparse(review_id, word, tf_idf)

# ============================= ADAPT DATA TO XGBOOST & CROSS VALIDATION ===================================
# create label
label <- df %>% 
  filter(review_id %in% rownames(feat))

# # For whatever reason a row in feat DOES NOT have its corresponding label
# sum(!(label$review_id %in% rownames(feat)))
# ind <- which(!rownames(feat) %in% label$review_id)
# feat <- feat[-ind, ]

# Create train and test
ind <- sample(1:nrow(label), round(nrow(label)*0.80), replace = FALSE)

train_feat <- feat[ind, ]
train_label <- label[ind, ]

test_feat <- feat[-ind, ]
test_label <- label[-ind, ]

# ============================= Train Models ===================================
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

# # model
# mod <- xgboost(data = train_feat,
#                label = train_label$rating,
#                nrounds = 100,
#                objective = "binary:logistic")
# 
# xgb.save(mod, 'model')
# mod <- xgb.load('model')

# # ============================= Get best cut off level  ===================================  
# ## Here we get the cut-off probability which makes the model's prediciton most accurate
# 
# # predict on train
# pred_train <- data_frame(prob = predict(mod, train_feat),
#                          bin = as.numeric(predict(mod, train_feat) > 0.5),
#                          actual = train_label$rating)
# 
# seq <- seq(0, 1, length = 100)
# 
# # data frame with accuracy for each cut probability
# acc_cut_train <- seq %>% 
#   map(~ pred_train$prob > .x) %>% 
#   map_int(~ sum(.x == pred_train$actual)) %>% 
#   data_frame(n_correct = ., seq) %>% 
#   mutate(accuracy = n_correct/nrow(pred_train))
# 
# accuracy_train_best <- acc_cut_train %>%
#   filter(n_correct == max(n_correct)) %>% 
#   select(accuracy) %>% 
#   .[[1]]
# 
# # isolate the cut probability that yields the best accuracy
# cut <- acc_cut_train %>% 
#   filter(n_correct == max(n_correct)) %>% 
#   select(seq) %>% 
#   .[[1]]
# 
# # ============================= PREDICT ON TEST SET ===================================
# pred_test <- data_frame(prob = predict(mod, test_feat),
#                         bin = as.numeric(predict(mod, test_feat) > cut),
#                         actual = test_label$rating)
# 
# accuracy_test <- sum(pred_test$bin == pred_test$actual)/nrow(pred_test)
# 
# # compare both
# paste("Accuracy in test is:",
#       round(accuracy_test, 2),
#       ".Whie in train it was:",
#       round(accuracy_train_best, 2))
# 
# # ============= What would be the best cut off in the test set =============
# acc_cut_test <- seq %>% 
#   map(~ pred_test$prob > .x) %>% 
#   map_int(~ sum(.x == pred_test$actual)) %>% 
#   data_frame(n_correct = ., seq) %>% 
#   mutate(accuracy = n_correct/nrow(pred_test))
# 
# accuracy_test_best <- acc_cut_test %>%
#   filter(n_correct == max(n_correct)) %>% 
#   select(accuracy) %>% 
#   .[[1]]
# 
# # ============================= Visualization ===================================
# acc_cut_train %>% 
#   ggplot(aes(seq, accuracy)) +
#   geom_point()
# 
# acc_cut_test %>%
#   ggplot(aes(seq, accuracy)) +
#   geom_point()