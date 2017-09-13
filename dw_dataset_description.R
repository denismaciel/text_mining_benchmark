### PREPARATION ----
# DOWNLOAD PACKAGES
if(!require("data.table")) install.packages("data.table"); library("data.table")
if(!require("tm")) install.packages("tm"); library(tm)
if(!require("tidytext")) install.packages("tidytext"); library(tidytext)
if(!require("lsa")) install.packages("lsa"); library("lsa")
if(!require("caret")) install.packages("caret"); library("caret")
if(!require("stringr")) install.packages("stringr"); library("stringr")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if(!require("reshape")) install.packages("reshape"); library("reshape")

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

# ============================= Dataset Description ===================================
tt <- to_be_cleaned %>%
  select(review_id, binary_rating, rating, review_text) %>%
  unnest_tokens(word, review_text) 


#POSITIVE AND NEGATIVE WORDS
tt %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  filter(sentiment == "negative") 
tt %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  filter(sentiment == "positive") 

tt %>% dim
tt %>% count(word, sort = TRUE)

#LENGTH OF REVIEW
word_count_per_review <- to_be_cleaned %>%
  unnest_tokens(word, review_text) %>%
  anti_join(stop_words) %>%
  group_by(word, review_id) %>%
  summarise(n = n()) %>%
  group_by(review_id) %>%
  summarise(n = n())
mean(as.numeric(word_count_per_review$n))

