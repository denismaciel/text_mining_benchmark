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
tdm <- TermDocumentMatrix(corpus, control = list(bounds = list(global = c(10, Inf)),
                                                 tokenize = NgramTokenizer))
#tdm <- TermDocumentMatrix(corpus, control = list(bounds = list(global = c(1, Inf)),tokenize = NgramTokenizer,dictionary=rownames(tdm_train)))



rm(NgramTokenizer)
rm(corpus)
# Reduce the sparcity of the matrix
# dim(dtm)
tdm <- removeSparseTerms(tdm, sparse = 0.99)
