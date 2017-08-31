library(data.table)
library(tidyverse)
library(stringr)
library(readxl)
library(broom)
              
source('~/Google Drive/Finance/R Projects/helper_functions.R')
#================================ Header ================================ 

df <- read_csv('data/AmazonBooks_sample.csv')

out <- paste("__label__", df$rating, " ", df$review_text, sep = "")

write_lines(out, 'data/fasttext_AmazonBooks_sample.txt')


df %>% 
  count(rating) %>% 
  mutate(n_pp = n/sum(n)*100)

