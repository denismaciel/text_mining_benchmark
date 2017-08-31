library(data.table)
library(tidyverse)
library(stringr)
library(readxl)
library(broom)
              
source('~/Google Drive/Finance/R Projects/helper_functions.R')
#================================ Header ================================
#============================= Extra Packages ===================================
if(!require("rword2vec")) install.packages("rword2vec"); library("rword2vec")
if(!require("text2vec")) install.packages("text2vec"); library("text2vec")
if(!require("glmnet")) install.packages("glmnet"); library("glmnet")

df <- readRDS("data/AmazonBooks.RDS") %>% tbl_df()

# transform to lower case and remove all punctuation
df$review_word2vec <- df$review_text %>% 
  tolower() %>% 
  str_replace_all("[]\\?!\"\'#$%&(){}+*/:;,._`|~\\[<=>@\\^-]", "")
  
#Separate data in train and test dataset
setDT(df)
setkey(df, review_id)

all_ids <- df$review_id
train_ids <- sample(all_ids, round(nrow(df)*0.8))
test_ids <- setdiff(all_ids, train_ids)
train <- df[J(train_ids)]
test <- df[J(test_ids)]

# Second Part

tok_fun <- word_tokenizer
it_train <- itoken(train$review_text, 
                   tokenizer = tok_fun, 
                   ids = train$review_id, 
                   progressbar = TRUE)
stop_words <- c("i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours")
vocab <- create_vocabulary(it_train, stopwords = stop_words, ngram = c(1L, 2L))
pruned_vocab <- prune_vocabulary(vocab, 
                                 term_count_min = 10, 
                                 doc_proportion_max = 0.5,
                                 doc_proportion_min = 0.001)
vectorizer <- vocab_vectorizer(pruned_vocab)

dtm_train <- create_dtm(it_train, vectorizer)
dtm_train <- normalize(dtm_train, "l1") #normalizes dtm_train so that every row sums up to 1


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

data("stop_words", package = "tidytext")

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

boo <- rownames(feat) %in% rownames(dtm_train)
train_tfidf <- feat[boo, ]
train_tfidf <- train_tfidf[order(rownames(train_tfidf)), ]

boo <- rownames(dtm_train) %in% rownames(train_tfidf)
dtm_train <- dtm_train[boo, ]
dtm_train <- train_tfidf[order(rownames(train_tfidf)), ]

table(rownames(dtm_train) == rownames(train_tfidf))

train_full <- cbind(train_tfidf, dtm_train)

# ============================= ADAPT DATA TO XGBOOST & CROSS VALIDATION ===================================
# create label
label_train <- df %>% 
  select(review_id, binary_rating) %>% 
  filter(review_id %in% rownames(train_full))

# model
mod_xgboost <- xgboost(data = train_full,
               label = label_train$binary_rating,
               nrounds = 100,
               objective = "binary:logistic")

mod_glmnet  <-  cv.glmnet(x = train_full, 
                          y = label_train$binary_rating, 
                          family = 'binomial', 
                          alpha = 1,
                          type.measure = "auc",
                          nfolds = 5,
                          thresh = 1e-3,
                          maxit = 1e3)

print(paste("max AUC =", round(max(glmnet_classifier$cvm), 5)))
plot(glmnet_classifier)

pred_train <- data_frame(actual = label_train$binary_rating,
                         xgboost = as.numeric(predict(mod_xgboost, train_full) > 0.5),
                         glmnet = as.numeric(predict(mod_glmnet, 
                                                     train_full,
                                                     type = "response") > 0.5))

pred_train %>% 
  count(actual, xgboost, glmnet, sort = TRUE) %>% 
  ungroup() %>% 
  mutate(n_pp = n/sum(n))

it_test <- test$review_text %>% 
  tok_fun() %>% 
  itoken(ids = test$review_id, 
         progressbar = FALSE)

dtm_test <- create_dtm(it_test, vectorizer)
dtm_test <- normalize(dtm_test, "l1") #normalizes dtm_test so that every row sums up to 1

print(paste("max AUC =", round(max(glmnet_classifier$cvm), 5)))
plot(glmnet_classifier)

preds <- predict(glmnet_classifier, dtm_test, type = 'response')[,1]
preds_bin <- as.numeric(predict(glmnet_classifier, dtm_test, type = 'response')[,1] > 0.5)
glmnet:::auc(test$sentiment, preds)

table(test$sentiment, preds_bin) %>% prop.table()*100

# pred_train %>% 
#   count(actual, bin) %>% 
#   ungroup() %>% 
#   mutate(n_pp = n/sum(n)*100)
# 
# pred_test %>% 
#   count(actual, bin) %>% 
#   ungroup() %>% 
#   mutate(n_pp = n/sum(n)*100)
