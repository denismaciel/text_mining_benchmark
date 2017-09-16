#feature hashing

# ============================= Header ===================================
# General Purpose Packages

if(!require("data.table")) install.packages("data.table"); library("data.table")
if(!require("tidyverse")) install.packages("tidyverse"); library("tidyverse")
if(!require("stringr")) install.packages("stringr"); library("stringr")
if(!require("Matrix")) install.packages("Matrix"); library("Matrix")
if(!require("text2vec")) install.packages("text2vec"); library("text2vec")


# ============================= Prepare the Data ===================================


# CHOOSE


# CHOOSE
# type <- "test"
#type <- "train"

# CHOOSE
#size = 100000
#size = 50000
#size = 20000
#size = 10000
#size = 5000
#size = 2500



for (i in 1:5){
  
  train <- readRDS("data_new/imdb_train.RDS")
  test <- readRDS("data_new/imdb_test.RDS")
  
  #ds_name <- "amazonBooks"
  #ds_name <- "amazonfinefood"
  ds_name <- "imdb"
  #ds_name <- "twitter"
  #ds_name <- "yelp" 
  type <- "train"
  list_cuts <- as.list(c(1000,2500,5000,10000,20000,50000, 100000))

    size <- list_cuts[[i]]


technique <- "vectorizationFeatureHashing"
file_name <- paste0("data_new/", ds_name, "_", type, ".RDS")

to_log <- list(
  technique = technique, 
  file_name = file_name,
  ds_name = ds_name,
  type = type,
  size = size
)

#subsetting
start_cleaning <- Sys.time()
train_ids <- sample(1:nrow(train), size)
df_sub <- train[train_ids,]
train <- df_sub

to_be_cleaned <- data.frame(row.names = (1:(nrow(train)+nrow(test))))
to_be_cleaned$review_id <- NA
to_be_cleaned$rating <- NA
to_be_cleaned$binary_rating <- NA
to_be_cleaned$review_text <- NA

to_be_cleaned[1:nrow(train),] <- train
to_be_cleaned[(nrow(train) + 1):(nrow(train)+nrow(test)),] <- test


test_dummy <- data.frame(row.names = (1:nrow(to_be_cleaned)))
test_dummy$review_id <- NA
test_dummy$review_id <- to_be_cleaned$review_id
test_dummy$type <- NA
test_dummy$type[1:size] <- 0
test_dummy$type[(size+1):nrow(test_dummy)] <- 1


source("dw_cleaning.R")
df <- cleaned_out



df <- merge.data.frame(df,test_dummy, by = "review_id")
df$type <- ifelse(df$type == 0,"train", "test")

remove(cleaned_out, df_sub, test_dummy)

df_int <- split ( df,df$type)
train <- df_int$train
test <- df_int$test

remove(df_int)
end_cleaning <- Sys.time()



#Mixed approach----
# Transform train
start_features <- Sys.time()
tok_fun <- word_tokenizer
it_train <- itoken(df$review_text, 
                   tokenizer = tok_fun, 
                   ids = df$review_id, 
                   progressbar = TRUE)
vocab <- create_vocabulary(it_train, ngram = c(1L, 2L))
pruned_vocab <- prune_vocabulary(vocab, 
                                 term_count_min = 10, 
                                 doc_proportion_max = 0.5,
                                 doc_proportion_min = 0.001)
h_vectorizer <-  hash_vectorizer(hash_size = 2 ^ 14, ngram = c(1L, 2L))

dtm <- create_dtm(it_train, h_vectorizer)
dtm <- normalize(dtm, "l1") #normalizes dtm_train so that every row sums up to 1

end_features <- Sys.time()

#Save thw train data
saveRDS(dtm, paste0("./features/", to_log$technique, "_", "mix_" ,ds_name, "_", type, "_", size, ".RDS"))

#save mixed log file
to_log$computation_time <- list(
  cleaning = list(end_cleaning = end_cleaning, 
                  start_cleaning = start_cleaning,
                  duration_cleaning = as.numeric(difftime(end_cleaning, start_cleaning, units = "secs"))),
    features = list(end_features = end_features, 
                  start_features = start_features,
                  duration_features = as.numeric(end_features - start_features, units = "secs"))
)
to_log

date <- str_replace_all(strftime(Sys.time() , "%Y-%m-%dT%H:%M:%S"), ":", "")
jsonlite::toJSON(to_log, pretty = T) %>% 
  write(paste0("log_features/", to_log$technique, "_", "mix_" ,ds_name, "_", type, "_", size, "_", date, ".log"))

remove(df,dtm, vocab, pruned_vocab)

#Blind approach----
# Transform train and test
start_features <- Sys.time()
tok_fun <- word_tokenizer
it_train <- itoken(train$review_text, 
                   tokenizer = tok_fun, 
                   ids = train$review_id, 
                   progressbar = TRUE)
vocab <- create_vocabulary(it_train, ngram = c(1L, 2L))
pruned_vocab <- prune_vocabulary(vocab, 
                                 term_count_min = 10, 
                                 doc_proportion_max = 0.5,
                                 doc_proportion_min = 0.001)

h_vectorizer <-  hash_vectorizer(hash_size = 2 ^ 14, ngram = c(1L, 2L))
dtm_train <- create_dtm(it_train, h_vectorizer)
dtm_train <- normalize(dtm_train, "l1") #normalizes dtm_train so that every row sums up to 1
end_features <- Sys.time()


#Save thw train data
saveRDS(dtm_train, paste0("./features/", to_log$technique, "_", "blind_", ds_name, "_", type, "_", size, ".RDS"))

#save training log file
to_log$computation_time <- list(
  cleaning = list(end_cleaning = end_cleaning, 
                  start_cleaning = start_cleaning,
                  duration_cleaning = as.numeric(difftime(end_cleaning, start_cleaning, units = "secs"))),
  features = list(end_features = end_features, 
                  start_features = start_features,
                  duration_features = as.numeric(end_features - start_features, units = "secs"))
)
to_log

date <- str_replace_all(strftime(Sys.time() , "%Y-%m-%dT%H:%M:%S"), ":", "")
jsonlite::toJSON(to_log, pretty = T) %>% 
  write(paste0("log_features/", to_log$technique, "_", "blind_",ds_name, "_", type, "_", size, "_", date, ".log"))


rm(list = ls(all = TRUE))

}


#blind test



test <- readRDS("data_new/yelp_test.RDS")
#ds_name <- "amazonBooks"
#ds_name <- "amazonfinefood"
#ds_name <- "imdb"
#ds_name <- "twitter"
ds_name <- "yelp" 

type <- "test"
size <- 5000

technique <- "vectorizationFeatureHashing"
file_name <- paste0("data_new/", ds_name, "_", type, ".RDS")

to_log <- list(
  technique = technique, 
  file_name = file_name,
  ds_name = ds_name,
  type = type,
  size = size
)
start_cleaning <- Sys.time()
to_be_cleaned <- test


source("dw_cleaning.R")
test <- cleaned_out
remove(cleaned_out)
end_cleaning <- Sys.time()

start_features <- Sys.time()
tok_fun <- word_tokenizer
it_train <- itoken(test$review_text, 
                   tokenizer = tok_fun, 
                   ids = test$review_id, 
                   progressbar = TRUE)
vocab <- create_vocabulary(it_train, ngram = c(1L, 2L))
pruned_vocab <- prune_vocabulary(vocab, 
                                 term_count_min = 10, 
                                 doc_proportion_max = 0.5,
                                 doc_proportion_min = 0.001)

h_vectorizer <-  hash_vectorizer(hash_size = 2 ^ 14, ngram = c(1L, 2L))
dtm_train <- create_dtm(it_train, h_vectorizer)
dtm_train <- normalize(dtm_train, "l1") #normalizes dtm_train so that every row sums up to 1
end_features <- Sys.time()


#Save thw test
saveRDS(dtm_train, paste0("./features/", to_log$technique, "_", "blind_" ,ds_name, "_", type, "_", size, ".RDS"))

#save test log file
to_log$computation_time <- list(
  cleaning = list(end_cleaning = end_cleaning, 
                  start_cleaning = start_cleaning,
                  duration_cleaning = as.numeric(difftime(end_cleaning, start_cleaning, units = "secs"))),
  features = list(end_features = end_features, 
                  start_features = start_features,
                  duration_features = as.numeric(end_features - start_features, units = "secs"))
)
to_log

date <- str_replace_all(strftime(Sys.time() , "%Y-%m-%dT%H:%M:%S"), ":", "")
jsonlite::toJSON(to_log, pretty = T) %>% 
  write(paste0("log_features/", to_log$technique, "_", "blind_", ds_name, "_", type, "_", size, "_", date, ".log"))

