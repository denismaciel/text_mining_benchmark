# CREATE CORPUS
reviews_source <- VectorSource(cleaned_out$review_text)
corpus <- VCorpus(reviews_source)
rm(reviews_source)

# Add 2- and 3grams function
NgramTokenizer <-  function(x){
  wordVec <- words(x)
  bigramVec <- unlist(lapply(ngrams(wordVec, 2), paste, collapse = " "), use.names = FALSE)
  # trigramVec <- unlist(lapply(ngrams(wordVec, 3), paste, collapse = " "), use.names = FALSE)
  return(c(wordVec, bigramVec)) # add trigramVec to the sequence if used
}

# CREATE DTM and REDUCE SPARCIRY
# When creating DTM exclude tokens that occur in less than 10 reviews
dtm <- DocumentTermMatrix(corpus, control = list(bounds = list(global = c(10, Inf)),
                                                 tokenize = NgramTokenizer))
rm(NgramTokenizer)
rm(corpus)
# Reduce the sparcity of the matrix
# dim(dtm)
dtm <- removeSparseTerms(dtm, sparse = 0.99)
# dim(dtm)

# # Check the result
# inspect(dtm[1:5, 1:10])
# findFreqTerms(dtm, lowfreq = 200, highfreq = Inf)
# head(sort(colSums(as.matrix(dtm)), decreasing = TRUE), 20)
# dtm <- as.matrix(dtm)