

if(approach == "mix"){
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
  # CREATE DTM and REDUCE SPARCITY
  # When creating DTM exclude tokens that occur in less than 10 reviews
  tdm_train <- TermDocumentMatrix(corpus, control = list(bounds = list(global = c(10, Inf)),tokenize = NgramTokenizer)) 
  rm(NgramTokenizer)
  rm(corpus)
  # Reduce the sparcity of the matrix
  # dim(dtm)
  tdm_train <- removeSparseTerms(tdm_train, sparse = 0.99)
  }







if(approach == "foldin"){
  
  # CREATE CORPUS
  reviews_source_train <- VectorSource(cleaned_out$review_text)
  corpus_train <- VCorpus(reviews_source_train)
  rm(reviews_source_train)
  
  
 
 
  reviews_source_test <- VectorSource(test_data$review_text)
  corpus_test <- VCorpus(reviews_source_test)
  rm(reviews_source_test)
  
  # Add 2- and 3grams function
  NgramTokenizer <-  function(x){
    wordVec <- words(x)
    bigramVec <- unlist(lapply(ngrams(wordVec, 2), paste, collapse = " "), use.names = FALSE)
    # trigramVec <- unlist(lapply(ngrams(wordVec, 3), paste, collapse = " "), use.names = FALSE)
    return(c(wordVec, bigramVec)) # add trigramVec to the sequence if used
  }
  
  
  tdm_train <- TermDocumentMatrix(corpus_train, control = list(bounds = list(global = c(10, Inf)),tokenize = NgramTokenizer)) 
  tdm_train <- removeSparseTerms(tdm_train, sparse = 0.99)
  tdm_test <- TermDocumentMatrix(corpus_test, control = list(bounds = list(global = c(10, Inf)),tokenize = NgramTokenizer,dictionary=rownames(tdm_train)))
  #tdm_test <- removeSparseTerms(tdm_test, sparse = 0.99)
  rm(NgramTokenizer)
  rm(corpus_train)
  rm(corpus_test)
  # Reduce the sparcity of the matrix
  # dim(dtm)
  
  
  }




