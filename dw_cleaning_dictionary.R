library(stringr)
library(tidytext)
library(tm)
if(!require("SnowballC")) install.packages("SnowballC"); library("SnowballC")

# to_be_cleaned <- readRDS("data_new/amazonfinefood_test.RDS")


# CLEANING PROCESS FOR DICTIONARY METHOD
df_long <- to_be_cleaned %>% 
  mutate(review_text = str_replace_all(review_text, "\\.", "\\. ")) %>% 
  mutate(review_text = str_replace_all(review_text, "-", " ")) %>% 
  unnest_tokens(word, review_text) %>% 
  group_by(review_id) %>% 
  mutate(word_order = row_number()) %>% 
  ungroup() %>% 
  anti_join(stop_words) %>% 
  arrange(review_id, word_order) 

cleaned_out <- df_long %>% 
  group_by(review_id, binary_rating, rating) %>% 
  summarise(review_text = paste0(word, collapse = " "))

rm(df_long)
rm(to_be_cleaned)


# cleaned_out$type <- "test"
# saveRDS(cleaned_out, paste0("./data_new/", "amazonfinefood_test", "_clean", ".RDS"))


