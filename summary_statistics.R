library(data.table)
library(tidyverse)
library(stringr)
library(readxl)
library(broom)
              
source('~/Google Drive/Finance/R Projects/helper_functions.R')
#================================ Header ================================
library(tidytext)

df <- read_rds("data/AmazonBooks.RDS") %>% tbl_df()

# number of different words used in all dataset (stop_words excluded)
word_count <- df %>% 
  unnest_tokens(word, review_text) %>%
  anti_join(stop_words) %>% 
  group_by(word) %>% 
  summarise(n = n())

# number of different per review used in all dataset (stop_words excluded)
word_count_per_review <- df %>% 
  unnest_tokens(word, review_text) %>%
  anti_join(stop_words) %>% 
  group_by(word, review_id) %>%
  summarise(n = n()) %>% 
  group_by(review_id) %>% 
  summarise(n = n())

median(word_count_per_review$n)
mean(word_count_per_review$n)
