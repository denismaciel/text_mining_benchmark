#============================= Extra Packages ===================================
if(!require("rword2vec")) install.packages("rword2vec"); library("rword2vec")
if(!require("text2vec")) install.packages("text2vec"); library("text2vec")

### PREPARATION ----
# DOWNLOAD PACKAGES
if(!require("tm")) install.packages("tm"); library(tm)
if(!require("tidytext")) install.packages("tidytext"); library(tidytext)
if(!require("data.table")) install.packages("data.table"); library(data.table)

to_log <- list(
  technique = technique,
  file_name = file_name,
  ds_name = ds_name,
  size = size,
  path_to_output = path_to_output
)

# DOWNLOAD DATA ----
raw_data <- readRDS(file_name)

# SPLIT DATA ----
ind_sample <- sample(1:nrow(raw_data), size = to_log$size)
to_be_cleaned <- raw_data[ind_sample, ]
rm(raw_data)

# CLEAN DATA ----
start_cleaning <- Sys.time()
source("dw_cleaning.R")
end_cleaning <- Sys.time()

cleaned_out$type <- "train"

# MERGE WITH 5K (FOR "MIX" APPROACH ONLY!)
test_name <- paste0("data_new/", ds_name, "_test_clean", ".RDS")
test_data <- readRDS(test_name)
cleaned_out <- bind_rows(cleaned_out, test_data)
rm(test_data)

cleaned_out
# ============================= Blind Approach ===================================
blind_train <- cleaned_out %>% filter(type == "train")
blind_test <- cleaned_out %>% filter(type == "test")
mix <- cleaned_out

create_vectorVocabulary_features <- function(df) {

  setDT(df)
  setkey(df, review_id)
  
  it_train <- itoken(df$review_text, 
                     tokenizer = word_tokenizer, 
                     ids = df$review_id, 
                     progressbar = TRUE)
  
  vocab <- create_vocabulary(it_train, ngram = c(1L, 2L))
  
  pruned_vocab <- prune_vocabulary(vocab, 
                                   term_count_min = 10, 
                                   doc_proportion_max = 0.5,
                                   doc_proportion_min = 0.001)
  vectorizer <- vocab_vectorizer(pruned_vocab)
  dtm <- create_dtm(it_train, vectorizer)
  #normalize so that every row sums up to 1
  dtm <- normalize(dtm, "l1") 
  return(dtm)
}

blind_train_featsparse <- create_vectorVocabulary_features(blind_train)
blind_test_featsparse <- create_vectorVocabulary_features(blind_test)
mix_featsparse <- create_vectorVocabulary_features(mix)

# ============================= Split Mix after feature creation ================
mix_test <- cleaned_out %>% filter(type == "test")
mix_train <- cleaned_out %>% filter(type == "train")

ind <- which(rownames(mix_featsparse) %in% mix_test$review_id)
mix_test_featsparse <- mix_featsparse[ind, ]

ind <- which(rownames(mix_featsparse) %in% mix_train$review_id)
mix_train_featsparse <- mix_featsparse[ind, ]

# ============================= Save to files ===================================

save_output <- function(df, approach, tt, feat_or_label){
  
  saveRDS(df,
          paste0(to_log$path_to_output,
                 paste(to_log$technique, approach, to_log$ds_name, tt, to_log$size, feat_or_label, sep = "_"),
                 ".RDS"))
  
}

# Blind
save_output(blind_test_featsparse, "blind", "test", "feat")
save_output(blind_train_featsparse, "blind", "train", "feat")
save_output(blind_train, "blind", "train", "label")
save_output(blind_test, "blind", "test", "label")
# Mix
save_output(mix_test_featsparse, "mix", "test", "feat")
save_output(mix_train_featsparse, "mix", "train", "feat")
save_output(mix_train, "mix", "train", "label")
save_output(mix_test, "mix", "test", "label")

rm(list = setdiff(ls(), c("loop_grid", "path_to_output", "METHOD")))
