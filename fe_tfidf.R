### PREPARATION ----
# DOWNLOAD PACKAGES
if(!require("data.table")) install.packages("data.table"); library("data.table")
if(!require("tm")) install.packages("tm"); library(tm)
if(!require("tidytext")) install.packages("tidytext"); library(tidytext)

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

# ==================== Function to Create TFIDF features =======================
create_tfidf_features <- function(df, cut) {
  
  df_tidy <- df %>% 
    unnest_tokens(word, review_text) %>% 
    count(review_id, word, sort = TRUE) %>% 
    group_by(review_id) %>% 
    mutate(rank = row_number(),
           total = sum(n),
           `term frequency` = n/total)
  
  df_tfidf <- df_tidy %>%
    bind_tf_idf(word, review_id, n) %>% 
    filter(tf_idf > cut)
    
  feat_sparse <- df_tfidf %>% 
    cast_sparse(review_id, word, tf_idf)
  
  return(feat_sparse)
}

# Sparse features
blind_train_featsparse <- create_tfidf_features(blind_train, cut = 0.03)
blind_test_featsparse <- create_tfidf_features(blind_test, cut = 0.03)
mix_featsparse <- create_tfidf_features(mix, cut = 0.03)

blind_train_label <- blind_train %>% select(review_id, binary_rating)

align_featsparse <- function(featsparse) featsparse[order(rownames(featsparse)), ]
align_label <- function(label_df, featsparse) {
  # keep only the observations that made the cut after data cleaning
  ind <- which(label_df$review_id %in% rownames(featsparse))
  
  label_df_out <- label_df[ind, ]  %>% 
    arrange(as.character(review_id))
  
  return(label_df_out)
}
test_alignment <- function(a, b) sum(rownames(a) == b$review_id) == nrow(a)

blind_train_featsparse <- align_featsparse(blind_train_featsparse)
blind_train <- align_label(blind_train, blind_train_featsparse)

blind_test_featsparse <- align_featsparse(blind_test_featsparse)
blind_test <- align_label(blind_test, blind_test_featsparse)

mix_featsparse <- align_featsparse(mix_featsparse)
mix <- align_label(mix, mix_featsparse)

if(!(test_alignment(blind_train_featsparse, blind_train) &
   test_alignment(blind_test_featsparse, blind_test) &
   test_alignment(mix_featsparse, mix))) {
  (stop("Feature and Labels are not alligned")) 
}

# ============================= Split Mix after feature creation ===================================
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

rm(list = setdiff(ls(), c("loop_grid", "path_to_output")))