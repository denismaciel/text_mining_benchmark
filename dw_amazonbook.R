library(data.table)
library(tidyverse)
library(stringr)
library(readxl)
library(broom)
              
source('~/Google Drive/Finance/R Projects/helper_functions.R')
#================================ Header ================================

library(jsonlite)

df <- read_lines('data_denis/review_sample.json')
df <- fromJSON(sprintf("[%s]", paste(df, collapse=","))) %>% tbl_df()

out <- df %>% 
  mutate(review_id = paste(reviewerID, asin, sep = '_')) %>% 
  select(review_id, rating = overall, review_text = reviewText) %>% 
  mutate(binary_rating = as.numeric(rating > 3)) %>% 
  select(review_id, rating, binary_rating, review_text)

# 20k from positive, 20k from negative
pos <- out %>% filter(binary_rating == 1)
ind <- sample(1:nrow(pos), 20000, replace = FALSE)
pos <- pos[ind, ]

neg <- out %>% filter(binary_rating == 0)
ind <- sample(1:nrow(neg), 20000, replace = FALSE)
neg[ind, ]

out <- bind_rows(pos, neg)

saveRDS(out, 'data/AmazonBooks.RDS')