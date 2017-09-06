### PREPARATION ----
# DOWNLOAD PACKAGES
if(!require("data.table")) install.packages("data.table"); library("data.table")
if(!require("tm")) install.packages("tm"); library(tm)
if(!require("tidytext")) install.packages("tidytext"); library(tidytext)

# DOWNLOAD DATA ----
# NB!!! Select the dataset you need
# to_be_cleaned <-readRDS("data_new/amazonBooks_test.rds")
# to_be_cleaned <-readRDS("data_new/amazonfinefood_test.rds")
# to_be_cleaned <-readRDS("data_new/imdb_test.RDS")
# to_be_cleaned <-readRDS("data_new/twitter_test.RDS")
# 
to_be_cleaned <-readRDS("data_new/amazonBooks_train.RDS")
# to_be_cleaned <-readRDS("data_new/amazonfinefood_train.rds")
# to_be_cleaned <-readRDS("data_new/imdb_train.RDS")
# to_be_cleaned <-readRDS("data_new/twitter_train.RDS")

# CLEAN DATA ----
source("dw_cleaning.R")

# SPLIT DATA ----
# Split data into 100K, 50K, 20K, 10K, 5K, 25H, 1K and 5K_test
# FOR 100K and 5K_test:
# sampled_out <- cleaned_out # CHOOSE WHEN 100K or 5K_test
  
# CHOOSE THE NECESSAARY SIZE (excepr for 100K or 5K_test)
ind_sample <- sample(1:nrow(cleaned_out), size = 50000)
# ind_sample <- sample(1:nrow(cleaned_out), size = 20000)
# ind_sample <- sample(1:nrow(cleaned_out), size = 10000)
# ind_sample <- sample(1:nrow(cleaned_out), size = 5000)
# ind_sample <- sample(1:nrow(cleaned_out), size = 2500)
# ind_sample <- sample(1:nrow(cleaned_out), size = 1000)

sampled_out <- cleaned_out[ind_sample, ]


# CREATE DTM ---
source("dw_dtm.R")


### LDA itself ----
if(!require("topicmodels")) install.packages("topicmodels"); library(topicmodels)

# Call Document Term Matrix
reviews <- dtm

# Check the number of empty documents in the matrix
rowTotals <- apply(reviews, 1, sum) # Find the sum of words in each Document
sum(rowTotals == 0) # Check if there are reviews in the corpus that doesn't contain any frequent terms
reviews <- reviews[rowTotals > 0, ] # Delete empty reviews  

# Fit the model
k <- 20
fit <- LDA(reviews, k, method = "Gibbs", control = list(seed = 123, verbose = 250, burnin = 500, iter = 500))

# GET Results
# # Take a preliminary look at the results
# terms(fit, 10)
# topics(fit)[1:20]
# ldaOut.topics <- as.matrix(topics(fit))
ldafeatures <- posterior(fit)
ldafeatures_final <- ldafeatures$topics # Probability of word being a part of a topic
mydata <- as.data.frame(mydata)
features <- mydata[,c("review_id","rating","binary_rating")]
rowTotals[rowTotals==0]
features <- features[rowTotals > 0, ] # Needed when some reviews were deleted in the process as empty
final <- cbind(features,ldafeatures_final)

# SAVE RESULTS
saveRDS(final, "features/LDA_amazonBooks_100K.RDS")


# #### Topic models ----
# library(ldatuning)
# # ldatuning to find optimal topics number
# kTuning <- FindTopicsNumber(
#   reviews,
#   topics = seq(from = 16, to = 24, by = 4),
#   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#   method = "Gibbs",
#   control = list(seed = 123),
#   mc.cores = 3L,
#   verbose = TRUE
# )
# FindTopicsNumber_plot(kTuning)
