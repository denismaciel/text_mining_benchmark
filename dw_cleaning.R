library(tidyverse)
library(stringr)
library(tidytext)
library(tm)
if(!require("SnowballC")) install.packages("SnowballC"); library("SnowballC")

# to_be_cleaned <- readRDS("data_new/imdb_train.RDS")

df_long <- to_be_cleaned %>% 
  mutate(review_text = str_replace_all(review_text, "\\.", "\\. ")) %>% 
  mutate(review_text = str_replace_all(review_text, "-", " ")) %>% 
  unnest_tokens(word, review_text) %>% 
  group_by(review_id) %>% 
  mutate(word_order = row_number()) %>% 
  ungroup() %>% 
  anti_join(stop_words) %>% 
  arrange(review_id, word_order) %>% 
  mutate(word_stem = SnowballC::wordStem(word, language = "english"))

cleaned_out <- df_long %>% 
  group_by(review_id, binary_rating, rating) %>% 
  summarise(review_text = paste0(word_stem, collapse = " "))

rm(df_long)
rm(to_be_cleaned)
