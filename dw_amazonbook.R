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
  select(review_id, rating = overall, review_text = reviewText)

write_csv(out, 'data/AmazonBooks_bigger_sample.csv')

length(test_label$review_id)
length(rownames(test_feat))
length(prob)
