#load the data
#install_github("mukul13/rword2vec")

if(!require("githubinstall")) install.packages("githubinstall"); library("githubinstall")
if(!require("data.table")) install.packages("data.table"); library("data.table")
if(!require("devtools")) install.packages("devtools"); library("devtools")
if(!require("rword2vec")) install.packages("rword2vec"); library("rword2vec")
if(!require("text2vec")) install.packages("text2vec"); library("text2vec")
if(!require("readr")) install.packages("readr"); library("readr")
if(!require("glmnet")) install.packages("glmnet"); library("glmnet")


mydata <- readRDS("data/imdb_df.Rds")


#transform to lower case and remove all punctuation
mydata$review_text <- tolower (mydata$review_text)
punct <- '[]\\?!\"\'#$%&(){}+*/:;,._`|~\\[<=>@\\^-]'
mydata$review_text <-  gsub( punct, "", x = mydata$review_text)

#Separate data in train and test dataset----
setDT(mydata)
setkey(mydata, review_id)
set.seed(2016L)
all_review_ids <- mydata$review_id
train_review_ids <- sample(all_review_ids, nrow(mydata)*0.8)
test_review_ids <- setdiff(all_review_ids, train_review_ids)
train <- mydata[J(train_review_ids)]
test <- mydata[J(test_review_ids)]

#Second part (working)----

tok_fun <- word_tokenizer
it_train <- itoken(train$review_text, 
                   tokenizer = tok_fun, 
                   ids = train$review_id, 
                   progressbar = FALSE)
stop_words <- c("i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours")
vocab <- create_vocabulary(it_train, stopwords = stop_words, ngram = c(1L, 2L))
pruned_vocab <- prune_vocabulary(vocab, 
                                term_count_min = 10, 
                                doc_proportion_max = 0.5,
                                doc_proportion_min = 0.001)
vectorizer <- vocab_vectorizer(pruned_vocab)


dtm_train <- create_dtm(it_train, vectorizer)
dtm_train <- normalize(dtm_train, "l1") #normalizes dtm_train so that every row sums up to 1



it_test <- test$review_text %>% 
  tok_fun %>% 
  itoken(ids = test$review_id, 
         progressbar = FALSE)

dtm_test <- create_dtm(it_test, vectorizer)
dtm_test <- normalize(dtm_test, "l1") #normalizes dtm_test so that every row sums up to 1

#Performing the training with logistic regression-----
NFOLDS = 5
glmnet_classifier = cv.glmnet(x = dtm_train, y = train[['binary_rating']], 
                              family = 'binomial', 
                              alpha = 1,
                              type.measure = "auc",
                              nfolds = NFOLDS,
                              thresh = 1e-3,
                              maxit = 1e3)
print(paste("max AUC =", round(max(glmnet_classifier$cvm), 5)))
plot(glmnet_classifier)


preds <- predict(glmnet_classifier, dtm_test, type = 'response')[,1]
glmnet:::auc(test$binary_rating, preds)


# Tf-IDF transformation based word2vec----
vocab <-  create_vocabulary(it_train)
vectorizer <- vocab_vectorizer(vocab)
dtm_train <- create_dtm(it_train, vectorizer)

tfidf <- TfIdf$new()

dtm_train_tfidf <- fit_transform(dtm_train, tfidf)


dtm_test_tfidf <- create_dtm(it_test, vectorizer) %>% 
  transform(tfidf)

t1 <- Sys.time()
glmnet_classifier <- cv.glmnet(x = dtm_train_tfidf, y = train[['binary_rating']], 
                              family = 'binomial', 
                              alpha = 1,
                              type.measure = "auc",
                              nfolds = NFOLDS,
                              thresh = 1e-3,
                              maxit = 1e3)
print(difftime(Sys.time(), t1, units = 'sec'))

plot(glmnet_classifier)
print(paste("max AUC =", round(max(glmnet_classifier$cvm), 4)))
preds <- predict(glmnet_classifier, dtm_test_tfidf, type = 'response')[,1]
glmnet:::auc(test$binary_rating, preds)





