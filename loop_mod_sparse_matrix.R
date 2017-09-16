library(data.table)
library(tidyverse)
library(stringr)

source('~/helper_functions/helper_functions.R')
#================================ Header ================================ 
# dataset <- "amazonBooks"
# technique <- "tfidf"
# size <- 50000

loop_technique <- c("vectorizationVocabulary")
folder <- paste0("./features/", loop_technique, "/")
loop_size <- c("1000",
               "2500",
               "5000",
               "10000",
               "20000",
               "50000",
               "1e+05")

loop_datasets <- c("amazonBooks",
                   "amazonfinefood",
                   "imdb",
                   "twitter",
                   "yelp")

loop_grid <- expand.grid(technique = loop_technique,
                         dataset = loop_datasets,
                         size = loop_size,
                         stringsAsFactors = FALSE)

# ============================= File Filtering ===================================
# remove 100k and 50k from imdb because they don't exist
loop_grid <- loop_grid %>% 
  filter(!(dataset == "imdb" & (size == "50000"| size == "1e+05"))) %>% 
  arrange(as.numeric(size))


# ======================= Here is where the magic happens: MODELS RUN =================

for (h in 1:nrow(loop_grid)) {
  
  technique <- loop_grid[h, ]$technique
  dataset <- loop_grid[h, ]$dataset
  size <- loop_grid[h, ]$size
  
  print(technique)
  print(dataset)
  print(size)
  print(h)
  source("./mod_sparse_matrices.R")
  
}

