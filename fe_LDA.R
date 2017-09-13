  ### PREPARATION ----
  # DOWNLOAD PACKAGES
  if(!require("data.table")) install.packages("data.table"); library("data.table")
  if(!require("tm")) install.packages("tm"); library(tm)
  if(!require("tidytext")) install.packages("tidytext"); library(tidytext)
  
  
  # SPECIFY SETTINGS----
  # NB!!! Select the dataset you need
  
  # CHOOSE
  # ds_name <- "amazonBooks"
  # ds_name <- "amazonfinefood"
  # ds_name <- "imdb"
  ds_name <- "twitter"
  # ds_name <- "yelp"
  
  # CHOOSE
  # type <- "test"
  type <- "train"
  
  # CHOOSE
  # size = 5000
  size = 100000
  # size = 50000
  # size = 20000
  # size = 10000
  # size = 5000
  # size = 2500
  # size = 1000
  
  # CHOOSE
  approach <- "mix"
  # approach <- "blind"
  
  
  technique <- "LDA"
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
  
  
  # MERGE WITH 5K (FOR "MIX" APPROACH ONLY!)
  if(approach == "mix"){
    cleaned_out$type <- "train"
    test_name <- paste0("data_new/", ds_name, "_test_clean", ".RDS")
    test_data <- readRDS(test_name)
    cleaned_out <- bind_rows(cleaned_out, test_data)
    rm(test_data)
  }
  
  
  # CREATE DTM ---
  start_dtm <- Sys.time()
  source("dw_dtm.R")
  end_dtm <- Sys.time()
  
  
  ### LDA itself ----
  start_features <- Sys.time()
  if(!require("topicmodels")) install.packages("topicmodels"); library(topicmodels)
  # Check the number of empty documents in the matrix
  rowTotals <- apply(dtm, 1, sum) # Find the sum of words in each Document
  sum(rowTotals == 0) # Check if there are reviews in the corpus that doesn't contain any frequent terms
  reviews <- dtm[rowTotals > 0, ] # Delete empty reviews
  rm(dtm)
  # Fit the model
  k <- 20
  fit <- LDA(reviews, k, method = "Gibbs", control = list(seed = 123, verbose = 250, burnin = 500, iter = 500))
  
  # GET Results
  # # Take a preliminary look at the results
  # terms(fit, 10)
  # topics(fit)[1:20]
  # ldaOut.topics <- as.matrix(topics(fit))
  ldafeatures <- posterior(fit)
  ldafeatures_final <- ldafeatures$topics # Probability of word being a part of a topic
  mydata <- as.data.frame(cleaned_out)
  if(approach == "mix"){
    features <- mydata[,c("review_id","rating","binary_rating","type")]
  }else{
    features <- mydata[,c("review_id","rating","binary_rating")]
  }
  rowTotals[rowTotals==0]
  features <- features[rowTotals > 0, ] # Needed when some reviews were deleted in the process as empty
  final <- cbind(features,ldafeatures_final)
  end_features <- Sys.time()
  
  # SAVE RESULTS----
  saveRDS(final, paste0("./features/", to_log$technique, "_", approach, "_", ds_name, "_", type, "_", size, "_", ".RDS"))
  
  
  # SAVE LOG ----
  to_log$computation_time <- list(
    cleaning = list(end_cleaning = end_cleaning, 
                    start_cleaning = start_cleaning,
                    duration_cleaning = as.numeric(difftime(end_cleaning, start_cleaning, units = "secs"))),
    dtm = list(end_dtm = end_dtm, 
               start_dtm = start_dtm,
               duration_dtm = as.numeric(difftime(end_dtm, start_dtm, units = "secs"))),
    features = list(end_features = end_features, 
                    start_features = start_features,
                    duration_features = as.numeric(end_features - start_features, units = "secs"))
  )
  to_log
  
  date <- str_replace_all(strftime(Sys.time() , "%Y-%m-%dT%H:%M:%S"), ":", "")
  jsonlite::toJSON(to_log, pretty = T) %>% 
    write(paste0("log_features/", to_log$technique, "_", approach, "_", ds_name, "_", type, "_", size, "_", date, ".log"))
