library(data.table)
library(tidyverse)
library(stringr)
#================================ Header ================================

# SPECIFY SETTINGS----
technique <- c("vectorizationVocabulary")
path_to_output <- "features/vectorizationVocabulary/"

ds_name <- c("amazonBooks",
             "amazonfinefood",
             "imdb",
             "twitter",
             "yelp")

size <- c(100000,
          50000,
          20000,
          10000,
          5000,
          2500,
          1000)


loop_grid <- expand.grid(technique = technique, 
                         ds_name = ds_name, 
                         size = size,
                         stringsAsFactors = FALSE)

# remove 100k and 50k from imdb because they don't exist
loop_grid <- loop_grid %>% 
  filter(!(ds_name == "imdb" & (size == "50000"| size == "1e+05"))) %>% 
  arrange(as.numeric(size))

for (h in 1:nrow(loop_grid)) {
  
  file_name <- paste0("data_new/", loop_grid[h, ]$ds_name, "_", "train", ".RDS")
  technique <- loop_grid[h, ]$technique
  ds_name <- loop_grid[h, ]$ds_name
  size <- loop_grid[h, ]$size
  
  print(technique)
  print(ds_name)
  print(size)
  print(h)
  if(technique == "tfidf"){
    print(technique)
    source("fe_tfidf.R")
  }
  if(technique == "vectorizationVocabulary") {
    print(technique) 
    source("fe_vectorizationVocabulary.R")
  }
}

