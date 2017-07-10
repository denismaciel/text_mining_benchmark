library(data.table)
library(tidyverse)
library(stringr)
library(readxl)
library(broom)
              
source('~/Google Drive/Finance/R Projects/helper_functions.R')
#================================ Header ================================

# ============================= VISUALIZATION ===================================

# uses pred_test as input

out <- data_frame(cut = numeric(), spec = numeric(), sens = numeric())
for (i in seq(0, 1, .01)){
  
  n_cor_pos <- pred_test %>% 
    mutate(aux = pred > i) %>% 
    filter(actual == aux,
           actual == 1) %>% 
    nrow()
  tot_pos <- pred_test %>% 
    filter(actual == 1) %>% 
    nrow()
  sens <- n_cor_pos/tot_pos
  
  n_cor_neg <- pred_test %>% 
    mutate(aux = pred > i) %>% 
    filter(actual == aux,
           actual == 0) %>% 
    nrow()
  tot_neg <- pred_test %>% 
    filter(actual == 0) %>% 
    nrow()
  spec <- n_cor_neg/tot_neg
  
  aux <- data_frame(cut = i, spec = spec, sens = sens)
  out <- bind_rows(out, aux)
  
}

out %>% 
  ggplot(aes(x = (1 - spec), y = sens)) +
  geom_point()

pred_test %>% 
  ggplot(aes(x = pred, fill = as.factor(actual))) +
  geom_histogram(alpha = 0.6, position = "identity") +
  facet_wrap(~ actual)