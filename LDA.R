
### PREPARATION ----
# DOWNLOAD PACKAGES
# if(!require("githubinstall")) install.packages("githubinstall"); library("githubinstall")
if(!require("data.table")) install.packages("data.table"); library("data.table")
# if(!require("devtools")) install.packages("devtools"); library("devtools")
# if(!require("rword2vec")) install.packages("rword2vec"); library("rword2vec")
# if(!require("text2vec")) install.packages("text2vec"); library("text2vec")
# if(!require("readr")) install.packages("readr"); library("readr")
# if(!require("glmnet")) install.packages("glmnet"); library("glmnet")
if(!require("tm")) install.packages("tm"); library(tm)


# DOWNLOAD DATA
# mydata <-readRDS("data/AmazonBooks.RDS")
mydata <-readRDS("data/twitter_50K.rds")


# Reduce the size to just 4000 reviews
# mydata <- mydata[1:4000,]


# CLEANING PROCEDURES FROM DJORDJE
# #transform to lower case and remove all punctuation
# mydata$review_text <- tolower (mydata$review_text)
# punct <- '[]\\?!\"\'#$%&(){}+*/:;,._`|~\\[<=>@\\^-]'
# mydata$review_text <-  gsub( punct, "", x = mydata$review_text)


# CREATE CORPUS
reviews_source <- VectorSource(mydata$review_text)
corpus <- VCorpus(reviews_source)


# CLEANING
# Transform all letters to lower-case
corpus <- tm_map(corpus, content_transformer(tolower))
# Remove all punctuation characters
replaceCharacter <- content_transformer(function(x, pattern, replacement)
  gsub(pattern = pattern,replacement = replacement, x))
# Here we make a decision to separate pizza-place into pizza place but wouldn't into wouldnt
corpus <- tm_map(corpus, replaceCharacter, "-", "")
corpus <- tm_map(corpus, replaceCharacter, "[[:punct:]]", " ")
# Reduce all whitespace to one and delete line breaks, etc.
corpus <- tm_map(corpus, stripWhitespace)
# Remove words without semantic content, like 'and' or 'it'
corpus <- tm_map(corpus, removeWords, stopwords("english"))
# Reduce all words to their word stem 
corpus <- tm_map(corpus, stemDocument, "english")
# Here we choose to ignore that there are French and German reviews in the corpus
# We can check out the content of each document like this
# corpus[[1]]$content


# DOCUMENT-TERM MATRIX
# During the process, we ignore terms that occur in less than five reviews
# because we expect them to be relevant for only a minor number of observations
dtm <- DocumentTermMatrix(corpus, control = list(bounds = list(global = c(5, Inf))))
# # inspect(dtm[1:10, 1:100])
# # We can have a look at frequent terms, e.g.
# # at terms occuring at least 100 times
# findFreqTerms(dtm, lowfreq = 200, highfreq = Inf)
# # We can add up the frequency of the terms over all documents
# # and have a look at the most frequent terms
# head(sort(colSums(as.matrix(dtm)), decreasing = TRUE), 20)
# We save this term document matrix and will use it later on
# saveRDS(dtm, "data/AmazonBooks_dtm.rds")


### Maybe execive ----
# # Compute n-grams, i.e. terms with more than one token
# # The core piece to this is function ngrams, that shifts a n-sized 
# # filter over the document and outputs words that occur together
# 
# # This function definition is taken from the tm documentation
# # It combines the list of ngrams from ngrams() and the single words
# BigramTokenizer <-  function(x){
#   wordVec <- words(x)
#   bigramVec <- unlist(lapply(ngrams(wordVec, 2), paste, collapse = " "), use.names = FALSE)
#   return(c(wordVec, bigramVec))
# }
# 
# # Apply it during the construction of the Document Term Matrix via option 'tokenize'.
# # This time, we will also apply a 'inverse document frequency' weighting to the 
# # word frequency matrix. By multiplying the frequency in the document with its inverse total 
# # frequency ratio, we put more weight on terms that are not very common. Intuitively,
# # common terms do not give us much info about a document


# tfIdf <- DocumentTermMatrix(corpus, control = list(bounds = list(global = c(8, Inf)),
#                                                    tokenize = BigramTokenizer,
#                                                    weighting = function(x) weightTfIdf(x, normalize = TRUE)))
# 
# # # We get a warning message that a document seems to be empty
# # corpus[[1537]]$content
# # # Indeed, the review is not in English and doesn't contain any words that come up in other reviews
# 
# # Let's look at the beginning of the full list of terms
# head(tfIdf$dimnames$Terms, 50)

 
# # It is common to reduce the number of rare tokens in the matrix
# # Dropping the rarest terms 1. increases computational speed and
# # 2. focuses on words that will likely appear in future reviews
# # The sparsity of a matrix or vector measures the percentage of 
# # empty (0) entries. If we want to use only terms that appear in 
# # at least 2% of reviews (here 40 reviews out of 2000), we should
# # set the maximum sparsity to 98%
# dim(tfIdf)
# tfIdf<- removeSparseTerms(tfIdf, sparse = 0.98)
# dim(tfIdf)

# # # To see if some 2-grams have made it through the selection
# # # process, since they are by definition rarer than their single word
# # # counterparts, we use grep to search the terms for a space " "
# # grep(tfIdf$dimnames$Terms, " ", value = TRUE)
 
# # We also save the tfIdf matrix and will use it later for sentiment analysis.
# saveRDS(tfIdf, "data/imdb_tfidf.rds")
 
# dtm <- as.matrix(dtm)
# tfIdf <- as.matrix(tfIdf)


### LAD itself ----
# We will work with the term-document matrix that we have created for the reviews
# reviews <- readRDS("./data/AmazonBooks_dtm.rds")
reviews <- dtm

# Have a quick look at reviews to see if everything worked
inspect(reviews[1:5, 1:10])

# Number of empty documents in the matrix
rowTotals <- apply(reviews, 1, sum) #Find the sum of words in each Document
sum(rowTotals == 0) # There is one review in the corpus that doesn't contain any freq. term
reviews <- reviews[rowTotals > 0, ]  

library(topicmodels)
# fitting the model
k <- 20
fit <- LDA(reviews, k, method = "Gibbs", control = list(seed = 123, verbose = 250, burnin = 500, iter = 500))

# results
terms(fit, 10)
topics(fit)[1:10]
ldaOut.topics <- as.matrix(topics(fit))
ldafeatures <- posterior(fit)
ldafeatures_final <- ldafeatures$topics
features <- mydata[,c("review_id","rating","binary_rating")]
# rowTotals[rowTotals==0]
# features <- features[rowTotals > 0, ] #Need when some lines were deleted in the process
final <- cbind(features,ldafeatures_final)
# saveRDS(final, "data/LDA_features_AmazonBooks.rds")
saveRDS(final, "data/LDA_features_twitter_50K.rds")

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
