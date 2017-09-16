library(data.table)
library(tidyverse)
library(stringr)

source('~/helper_functions/helper_functions.R')
#================================ Header ================================ 


# CHOOSE ONE METHOD:
METHOD <- "blind_and_mix"
# METHOD <- "foldin_only"
# METHOD <- "mix_only"

folder <- "/Users/denismaciel/Desktop/word2vec/"
loop_technique <- c("word2vecVectorAveraging")

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

# Check if all train files are available in the folder
x <- loop_grid %>%
  mutate(file_path = paste0(paste(technique,
                                  "mix", # either mix or foldin
                                  dataset,
                                  "train",
                                  size,
                                  sep = "_"),
                            ".RDS"))

real_files <- list.files(folder)
boo <- x$file_path %in% real_files
missing <- x[!boo, ]
print(missing)

# # If YOU WANT, remove the files that are not in the folder and run the model without them
# # BUT YOU BETTER DISCOVER WHAT'S THE REASON THEY'RE MISSING
# x <- x[boo, ]
# loop_grid <- x

## IF YOU WANT,first check if all files can be opened by R wtihtout error
# for (i in real_files){
#   df <- readRDS(paste0(folder, i))
#   rm(df)
# }
# h =2

# ======================= Here is where the magic happens: MODELS RUN =================

for (h in 1:nrow(loop_grid)) {
  
  technique <- loop_grid[h, ]$technique
  dataset <- loop_grid[h, ]$dataset
  size <- loop_grid[h, ]$size
  
  print(technique)
  print(dataset)
  print(size)
  print(h)
  if (METHOD == "blind_and_mix") source("./mod_framework.R")
  if (METHOD == "foldin_only") source("./mod_foldin.R")
  if (METHOD == "mix_only") source("./mod_mix.R")
  
}
  
