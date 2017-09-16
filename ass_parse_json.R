library(data.table)
library(tidyverse)
library(stringr)

source('~/helper_functions/helper_functions.R')
#================================ Header ================================

library(jsonlite)

json_files <- list.files("./log/", pattern = ".log", full.names = TRUE)

mother_of_all_lists <- list()

ix <- 0
for (json_file in json_files){
  
  ix <- ix + 1
  json <- fromJSON(json_file)
  
  
  # Add the name of approach to nested dfs
  for (i in 1:length(json$results)) {
    for (j in 1:length(json$results[[i]])){
      json$results[[i]][[j]]$approach <- names(json$results[i])
    }
  }
  
  rm(i, j)
  
  # Spread the data to paste it together
  l <- list()
  for (i in 1:length(json$results)){
    for (j in 1:length(json$results[[i]])){
      df_sparse <- json$results[[i]][[j]] %>% gather(key = metric, value, -model, -approach)
      l_length <- length(l) + 1 
      l[[l_length]] <- df_sparse
    }
  }
  
  l_df <- rbindlist(l)
  l_df$technique <- json$file_path$technique
  l_df$dataset <- json$file_path$dataset
  l_df$size <- json$file_path$size
  
  mother_of_all_lists[[ix]] <- l_df
  
}

df <- as_tibble(rbindlist(mother_of_all_lists))

df <- df %>% mutate(size = as.numeric(size), 
                    value = as.numeric(value))

df <- df %>% 
  group_by(model, approach, metric, technique, dataset, size) %>% 
  summarise(value = mean(value)) %>% 
  ungroup()

# ============================= Vizualization ===================================
p <- df %>% 
  filter(metric == "auc",
         model == "xgboost") %>% 
  ggplot(aes(x = size, y = value, colour = approach)) + 
  geom_line() +
  geom_point() +
  facet_wrap(~ dataset)

ggsave(p,filename =  "viz/LDA_learning_curve.jpg")



