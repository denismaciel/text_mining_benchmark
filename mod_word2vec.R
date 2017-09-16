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

# Transform train and test
# Train
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

# Test
it_test <- test$review_text %>% 
  tok_fun %>% 
  itoken(ids = test$review_id, 
         progressbar = TRUE)

dtm_test <- create_dtm(it_test, vectorizer)
dtm_test <- normalize(dtm_test, "l1") #normalizes dtm_test so that every row sums up to 1
