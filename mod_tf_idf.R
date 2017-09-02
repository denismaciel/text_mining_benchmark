# ============================= Header ===================================
# General Purpose Packages
library(data.table)
library(tidyverse)
library(stringr)
# Packages to prepare the data
library(Matrix)
library(tidytext)
library(tm)
# Packages to train the model
library(xgboost)
library(glmnet)

# ============================= Prepare the Data ===================================
df <- readRDS("data/yelp_reviews.rds")

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

feat_sparse <- df_tfidf %>% 
  cast_sparse(review_id, word, tf_idf)

label <- df %>% 
  filter(review_id %in% rownames(feat_sparse))

# ============================= Split Train and Test Sets =============================
# # For whatever reason a row in feat DOES NOT have its corresponding label
# sum(!(label$review_id %in% rownames(feat)))
# ind <- which(!rownames(feat) %in% label$review_id)
# feat <- feat[-ind, ]

ind <- sample(1:nrow(label), round(nrow(label)*0.80), replace = FALSE)

train_feat <- feat_sparse[ind, ]
train_label <- label[ind, ]

test_feat <- feat_sparse[-ind, ]
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

# ============================= Predict on Test Set ===================================
pred_test <- data_frame(pred_glmnet = predict(mod_glmnet, test_feat, type = 'response')[, 1],
                        pred_xgboost = predict(mod_xgboost, test_feat, type = 'response'),
                        actual = test_label$binary_rating)

