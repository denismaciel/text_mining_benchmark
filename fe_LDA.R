
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
rm(to_be_cleaned)

# CREATE DTM ---
source("dw_dtm.R")




# # str_count(cleaned_dtm$word, " ") %>% table()
# # 
# # trigramVec <- cleaned_out %>% 
# #   unnest_tokens(word, review_text, token = "ngrams", n=3)
# # 
# # bigramVec <- cleaned_out %>% 
# #   unnest_tokens(word, review_text, token = "ngrams", n=2)
# # 
# # wordVec <- cleaned_dtm <- cleaned_out %>% 
# #   unnest_tokens(word, review_text, token = "words")
# # 
# # a <- bind_rows(trigramVec,bigramVec,wordVec)
# # 
# # dtm <- a %>% 
# #   count(review_id, word, sort = TRUE) %>% 
# #   cast_dtm(review_id, word, n)
# 
# 
# 
# # CREATE CORPUS
# reviews_source <- VectorSource(mydata$review_text)
# corpus <- VCorpus(reviews_source)
# 
# # CLEAN CORPUS
# # Transform all letters to lower-case
# corpus <- tm_map(corpus, content_transformer(tolower))
# # Remove all punctuation characters
# replaceCharacter <- content_transformer(function(x, pattern, replacement)
#   gsub(pattern = pattern,replacement = replacement, x))
# # Address words with "-" and "´"
# corpus <- tm_map(corpus, replaceCharacter, "-", " ")
# corpus <- tm_map(corpus, replaceCharacter, "[[:punct:]]", "")
# # Reduce all whitespace to one and delete line breaks, etc.
# corpus <- tm_map(corpus, stripWhitespace)
# # Remove stopwords
# corpus <- tm_map(corpus, removeWords, stopwords("english"))
# # Reduce all words to their word stem 
# corpus <- tm_map(corpus, stemDocument, "english")
# # Check the content of a review
# corpus[[137]]$content

# CREATE a DTM and REDUCE SPARCITY
reviews_source <- VectorSource(mydata$review_text)
corpus <- VCorpus(reviews_source)
# Add 2- and 3grams
NgramTokenizer <-  function(x){
  wordVec <- words(x)
  bigramVec <- unlist(lapply(ngrams(wordVec, 2), paste, collapse = " "), use.names = FALSE)
  trigramVec <- unlist(lapply(ngrams(wordVec, 3), paste, collapse = " "), use.names = FALSE)
  return(c(wordVec, bigramVec, trigramVec))
}
# Create DTM and exclude terms that occur in less than 5 reviews
# because we expect them to be relevant for only a minor number of observations
dtm <- DocumentTermMatrix(corpus, control = list(bounds = list(global = c(5, Inf)),
                                                   tokenize = NgramTokenizer))
# Reduce the number of rare tokens in the matrix
dim(dtm)
dtm <- removeSparseTerms(dtm, sparse = 0.995)
dim(dtm)
# # Check the result
# inspect(dtm[1:5, 1:10])
# findFreqTerms(dtm, lowfreq = 200, highfreq = Inf)
# head(sort(colSums(as.matrix(dtm)), decreasing = TRUE), 20)
# dtm <- as.matrix(dtm)


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
# saveRDS(final, "data/LDA_features_AmazonBooks.rds")
# saveRDS(final, "data/LDA_features_twitter_50K.rds")
# saveRDS(final, "data/LDA_features_yelp.rds")
saveRDS(final, "data/LDA_features_imdb.rds")


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
