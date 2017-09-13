#REMEMBER TO SAVE  tdm_train <- tdm after first run if blind approach (fold in)
#AND IN SECOND RUN : set dictionary=rownames(tdm_train) when creating TermDocumentMatrix for test set (in dw_dtm_lsa.R)

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
# size = 10000
# size = 5000
# size = 2500
 size = 1000


technique <- "LSA"
file_name <- paste0("data_new/", ds_name, "_", type, ".RDS")

to_log <- list(
  technique = technique, 
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


# CREATE TextDocumentMatrix ---
start_tdm <- Sys.time()
source("dw_dtm_lsa.R")
end_tdm <- Sys.time()




# ============================= Creating Features ===================================


#create TermDocumentMatrix
start_features <- Sys.time()
myMatrix <- tdm
myMatrix = as.matrix(myMatrix)

#create latent semantic space
myMatrix.lsa = lsa(myMatrix,dimcalc_share())
myMatrix.lsa_tk=as.data.frame(myMatrix.lsa$tk)
myMatrix.lsa_dk=as.data.frame(myMatrix.lsa$dk)
myMatrix.lsa_sk=as.data.frame(myMatrix.lsa$sk)

#create features from discovered latent concepts
cr_feature <- myMatrix.lsa_dk
cr_feature$binary_rating <- cleaned_out$binary_rating
cr_feature$rating <- cleaned_out$rating
cr_feature$review_id <- cleaned_out$review_id

end_features <- Sys.time()

# SAVE RESULTS----
saveRDS(cr_feature, paste0("./features/", to_log$technique, "_", ds_name, "_", type, "_", size, ".RDS"))


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
  write(paste0("log_features/", to_log$technique, "_", ds_name, "_", type, "_", size, "_", date, ".log"))
