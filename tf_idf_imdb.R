library(data.table)
library(tidyverse)
library(stringr)
library(readxl)
library(broom)

source('~/Google Drive/Finance/R Projects/helper_functions.R')
#================================ Header ================================

library(tidytext)
library(xgboost)
library(tm)
library(Matrix)

# df <- read_csv('data/AmazonBooks_sample.csv')
df <- fread('data/imdb_df.tsv')
colnames(df) <- c('review_id', 'rating', 'review_text')
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
  select(review_id, rating) %>% 
  filter(review_id %in% rownames(feat))

# sum(!(label$review_id %in% rownames(feat)))

# # For whatever reason a row in feat DOES NOT have its corresponding label
# ind <- which(!rownames(feat) %in% label$review_id)
# feat <- feat[-ind, ]

# data to be fed into the model
ind <- sample(1:nrow(label), round(nrow(label)*0.80), replace = FALSE)

train_feat <- feat[ind, ]
train_label <- label[ind, ]

test_feat <- feat[-ind, ]
test_label <- label[-ind, ]

# model
mod <- xgboost(data = train_feat,
               label = train_label$rating,
               nrounds = 100,
               objective = "binary:logistic")

# ============================= Predict on the train ===================================

pred_train <- data_frame(prob = predict(mod, train_feat),
                         bin = as.numeric(predict(mod, train_feat) > 0.5),
                         actual = train_label$rating)

# Get best cut off level
## Here we get the cut-off probability which makes the model's prediciton most accurate

l <- list()

seq <- seq(0, 1, length = 100)

for (k in seq){
  
  pred <- as.numeric(pred_train$prob > k)
  
  accuracy_train <- table(pred = pred, actual = pred_train$actual) %>% 
    prop.table() %>% 
    broom::tidy() %>% 
    filter(as.character(pred) == as.character(actual)) %>% # as.character to get rid of factors
    summarise(sum(Freq)) %>% 
    .[[1]]
  
  ind <- which(near(seq, k)) # get the index to insert it into a list
  l[[ind]] <- data.frame(prob = k, accuracy_train)
  print(ind)
  
}

cuts <- data.table::rbindlist(l)

# THE BEST CUT
cut <- cuts %>%
  filter(accuracy_train == max(accuracy_train)) %>% 
  select(prob) %>% 
  .[[1]]

accuracy_train_best <- cuts %>%
  filter(accuracy_train == max(accuracy_train)) %>% 
  select(accuracy_train) %>% 
  .[[1]]

# ============================= PREDICT ON TEST SET ===================================
pred_test <- data_frame(prob = predict(mod, test_feat),
                   bin = as.numeric(predict(mod, test_feat) > cut),
                   actual = test_label$rating)

accuracy_test <- table(pred = pred_test$bin, actual = pred_test$actual) %>% 
  prop.table() %>% 
  broom::tidy() %>% 
  filter(as.character(pred) == as.character(actual)) %>% # as.character to get rid of factors
  summarise(sum(Freq)) %>% 
  .[[1]]

# compare both
paste("Accuracy in test is:",
      round(accuracy_test, 2),
      ".Whie in train it was:",
      round(accuracy_train_best, 2))

# ============================= What would be the best cut off in the test set ===================================

l <- list()

seq <- seq(0, 1, length = 100)

for (k in seq){
  
  pred <- as.numeric(pred_test$prob > k)
  
  accuracy_testz <- table(pred = pred, actual = pred_test$actual) %>% 
    prop.table() %>% 
    broom::tidy() %>% 
    filter(as.character(pred) == as.character(actual)) %>% # as.character to get rid of factors
    summarise(sum(Freq)) %>% 
    .[[1]]
  
  ind <- which(near(seq, k)) # get the index to insert it into a list
  l[[ind]] <- data.frame(prob = k, accuracy_testz)
  print(ind)
  
}

cuts_testz <- data.table::rbindlist(l)

# THE BEST CUT
cut_testz <- cuts_testz %>%
  filter(accuracy_testz == max(accuracy_testz)) %>% 
  select(prob) %>% 
  .[[1]]

accuracy_train_best <- cuts %>%
  filter(accuracy_train == max(accuracy_train)) %>% 
  select(accuracy_train) %>% 
  .[[1]]

cuts %>% 
  ggplot(aes(x = 1:nrow(cuts), y = accuracy_train)) +
  geom_point()

cuts_testz %>% 
  ggplot(aes(x = 1:nrow(cuts_testz), y = accuracy_testz)) +
  geom_point()