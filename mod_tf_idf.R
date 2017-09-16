### PREPARATION ----
# DOWNLOAD PACKAGES
if(!require("data.table")) install.packages("data.table"); library("data.table")
if(!require("tm")) install.packages("tm"); library(tm)
if(!require("tidytext")) install.packages("tidytext"); library(tidytext)


# SPECIFY SETTINGS----
# NB!!! Select the dataset you need

technique <- "tfidf"

# CHOOSE
ds_name <- "amazonBooks"
# ds_name <- "amazonfinefood"
# ds_name <- "imdb"
# ds_name <- "twitter"
# ds_name <- "yelp"

# CHOOSE
# type <- "test"
type <- "train"

# CHOOSE
# size = 5000
# size = 100000
# size = 50000
# size = 20000
# size = 10000
# size = 5000
size = 2500
# size = 1000

# CHOOSE
# approach <- "mix"
approach <- "blind"

file_name <- paste0("data_new/", ds_name, "_", type, ".RDS")

to_log <- list(
  technique = technique,
  approach = approach,
  file_name = file_name,
  ds_name = ds_name,
  type = type,
  size = size
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
if(approach == "mix"){
  test_name <- paste0("data_new/", ds_name, "_test_clean", ".RDS")
  test_data <- readRDS(test_name)
  cleaned_out <- bind_rows(cleaned_out, test_data)
  rm(test_data)
}


# CREATE DTM ---
start_dtm <- Sys.time()
source("dw_dtm.R")
end_dtm <- Sys.time()




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

