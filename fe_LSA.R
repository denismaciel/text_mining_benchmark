
### PREPARATION ----
# DOWNLOAD PACKAGES
if(!require("data.table")) install.packages("data.table"); library("data.table")
if(!require("tm")) install.packages("tm"); library(tm)
if(!require("tidytext")) install.packages("tidytext"); library(tidytext)
if(!require("lsa")) install.packages("lsa"); library("lsa")
if(!require("caret")) install.packages("caret"); library("caret")

# SPECIFY SETTINGS----
# NB!!! Select the dataset you need

# CHOOSE
#ds_name <- "amazonBooks"
# ds_name <- "amazonfinefood"
# ds_name <- "imdb"
# ds_name <- "twitter"
 ds_name <- "yelp"

# CHOOSE
# type <- "test"
type <- "train"

# CHOOSE
# size = 5000
# size = 100000
# size = 50000
# size = 20000
 size = 10000
# size = 5000
# size = 2500
# size = 1000

# CHOOSE
#approach <- "mix"
 approach <- "foldin" 
 

technique <- "LSA"
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

test_name <- paste0("data_new/", ds_name, "_test_clean", ".RDS")
test_data <- readRDS(test_name)

# MERGE WITH 5K (FOR "MIX" APPROACH ONLY!)
if(approach == "mix"){  
  cleaned_out <- bind_rows(cleaned_out, test_data)
  rm(test_data)
}


# CREATE TextDocumentMatrix ---
start_tdm <- Sys.time()
source("dw_dtm_lsa.R")
end_tdm <- Sys.time()




# ============================= Creating Features ===================================

if(approach == "mix"){
#create TermDocumentMatrix
start_features <- Sys.time()
myMatrix <- tdm_train
myMatrix = as.matrix(myMatrix)

#create latent semantic space
myMatrix.lsa = lsa(myMatrix,dimcalc_share())
projectedmatrix = t(as.textmatrix(myMatrix.lsa))
projectedmatrix <-matrix(projectedmatrix, nrow = dim(projectedmatrix)[1],
                         ncol = dim(projectedmatrix)[2])

projectedmatrix <- as.data.table(projectedmatrix)
projectedmatrix$binary_rating <- cleaned_out$binary_rating
projectedmatrix$rating <- cleaned_out$rating
projectedmatrix$review_id <- cleaned_out$review_id
projectedmatrix$type <- cleaned_out$type


end_features <- Sys.time()
}

if(approach == "foldin"){
  #create TermDocumentMatrix
  start_features <- Sys.time()
  myMatrix <- tdm_train
  myMatrix = as.matrix(myMatrix)
  #create latent semantic space
  myMatrix.lsa = lsa(myMatrix,dimcalc_share())
  projectedmatrix.train = t(as.textmatrix(myMatrix.lsa))
  projectedmatrix.train <-matrix(projectedmatrix.train, nrow = dim(projectedmatrix.train)[1],
                                  ncol = dim(projectedmatrix.train)[2])
  projectedmatrix.train <- as.data.table(projectedmatrix.train)
  projectedmatrix.train$binary_rating <- cleaned_out$binary_rating
  projectedmatrix.train$rating <- cleaned_out$rating
  projectedmatrix.train$review_id <- cleaned_out$review_id
  projectedmatrix.train$type <- cleaned_out$type
  
  projected.test.lsa = fold_in(docvecs = as.matrix(tdm_test), LSAspace = myMatrix.lsa)
  projectedmatrix.test <- t(projected.test.lsa)
  projectedmatrix.test <-matrix(projectedmatrix.test, nrow = dim(projectedmatrix.test)[1],
                                 ncol = dim(projectedmatrix.test)[2])
  projectedmatrix.test <- as.data.table(projectedmatrix.test)
  projectedmatrix.test$binary_rating <- test_data$binary_rating
  projectedmatrix.test$rating <- test_data$rating
  projectedmatrix.test$review_id <- test_data$review_id
  projectedmatrix.test$type <- test_data$type
  
  projectedmatrix <- rbind(projectedmatrix.train,projectedmatrix.test)
  end_features <- Sys.time()
}

# SAVE RESULTS----
saveRDS(projectedmatrix, paste0("./features/", to_log$technique, "_", approach, "_", ds_name, "_", type, "_", size, ".RDS"))


# SAVE LOG ----
to_log$computation_time <- list(
  cleaning = list(end_cleaning = end_cleaning, 
                  start_cleaning = start_cleaning,
                  duration_cleaning = as.numeric(difftime(end_cleaning, start_cleaning, units = "secs"))),
  tdm = list(end_tdm = end_tdm, 
             start_tdm = start_tdm,
             duration_tdm = as.numeric(difftime(end_tdm, start_tdm, units = "secs"))),
  features = list(end_features = end_features, 
                  start_features = start_features,
                  duration_features = as.numeric(end_features - start_features, units = "secs"))
)
to_log

date <- str_replace_all(strftime(Sys.time() , "%Y-%m-%dT%H:%M:%S"), ":", "")
jsonlite::toJSON(to_log, pretty = T) %>% 
  write(paste0("log_features/", to_log$technique, "_", approach, "_", ds_name, "_", type, "_", size, "_", date, ".log"))
