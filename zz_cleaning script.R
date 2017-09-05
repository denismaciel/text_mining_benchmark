# Key cleaning procedures
if(!require("tm")) install.packages("tm"); library(tm)
mydata <-readRDS("data/###.Rds") 

# CREATE CORPUS 
reviews_source <- VectorSource(mydata$review_text)
corpus <- VCorpus(reviews_source)

# CLEAN CORPUS
# Transform all letters to lower-case
corpus <- tm_map(corpus, content_transformer(tolower))
# Remove all punctuation characters
replaceCharacter <- content_transformer(function(x, pattern, replacement)
  gsub(pattern = pattern,replacement = replacement, x))
# Address words with "-" and "´"
corpus <- tm_map(corpus, replaceCharacter, "-", " ")
corpus <- tm_map(corpus, replaceCharacter, "[[:punct:]]", "")
# Reduce all whitespace to one and delete line breaks, etc.
corpus <- tm_map(corpus, stripWhitespace)
# Remove stopwords
corpus <- tm_map(corpus, removeWords, stopwords("english"))
# Reduce all words to their word stem 
corpus <- tm_map(corpus, stemDocument, "english")
# Check the content of a review
# corpus[[137]]$content

# CREATE a DOCUMENT-TERM MATRIX and EXCLUDE RARE WORDS
# Add function for creation of 2- and 3grams
NgramTokenizer <-  function(x){
  wordVec <- words(x)
  bigramVec <- unlist(lapply(ngrams(wordVec, 2), paste, collapse = " "), use.names = FALSE)
  trigramVec <- unlist(lapply(ngrams(wordVec, 3), paste, collapse = " "), use.names = FALSE)
  return(c(wordVec, bigramVec, trigramVec))
}

# Create document term matrix and exclude terms that occur in less than 5 reviews
dtm <- DocumentTermMatrix(corpus, control = list(bounds = list(global = c(5, Inf)),
                                                 tokenize = NgramTokenizer))

# Reduce the number of rare tokens in the matrix
# dim(dtm)
dtm <- removeSparseTerms(dtm, sparse = 0.99)
# dim(dtm)


# # Should be used to add to or substitute the folllowing: ----
# # below just some documents with cleaning procedures
# # In 1stinsp_text2vec
# prep_fun = function(x) {
#   x %>% 
#     # make text lower case
#     str_to_lower %>% 
#     # remove non-alphanumeric symbols
#     str_replace_all("[^[:alnum:]]", " ") %>% 
#     # collapse multiple spaces
#     str_replace_all("\\s+", " ")
# }
# 
# 
# # In Grand Model
# tok_fun <- word_tokenizer
# it_train <- itoken(train$review_text, 
#                    tokenizer = tok_fun, 
#                    ids = train$review_id, 
#                    progressbar = TRUE)
# stop_words <- c("i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours")
# vocab <- create_vocabulary(it_train, stopwords = stop_words, ngram = c(1L, 2L))
# pruned_vocab <- prune_vocabulary(vocab, 
#                                  term_count_min = 10, 
#                                  doc_proportion_max = 0.5,
#                                  doc_proportion_min = 0.001)
# 
# 
# # In word2vec
# # transform to lower case and remove all punctuation
# df$review_word2vec <- df$review_text %>% 
#   tolower() %>% 
#   str_replace_all("[]\\?!\"\'#$%&(){}+*/:;,._`|~\\[<=>@\\^-]", "")
# 
# stop_words <- c("i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours")
# 
# 
# #transform to lower case and remove all punctuation
# mydata$review_text <- tolower (mydata$review_text)
# punct <- '[]\\?!\"\'#$%&(){}+*/:;,._`|~\\[<=>@\\^-]'
# mydata$review_text <-  gsub( punct, "", x = mydata$review_text)