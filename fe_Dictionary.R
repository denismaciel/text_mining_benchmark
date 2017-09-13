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
# ds_name <- "amazonBooks"
 ds_name <- "amazonfinefood"
# ds_name <- "imdb"
# ds_name <- "twitter"
# ds_name <- "yelp"

# CHOOSE
type <- "test"
# type <- "train"

# CHOOSE
 size = 5000
# size = 100000
# size = 50000
# size = 20000
# size = 10000
# size = 5000
# size = 2500
# size = 1000



technique <- "Dictionary"
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

