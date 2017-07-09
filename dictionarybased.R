setwd("/Users/yanyan/Documents/APA_textmining")

library("plyr")
library(reshape)
library("tm")
library('syuzhet')
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("SentimentAnalysis")
library("rjson")
library("RJSONIO")
library("tidytext")
library(reshape2)
library(caret)
library(e1071)
library('pROC')
#--------------------------------------------------------------------------
# Prepare dictionary
lexicon <- sentiments[sentiments$lexicon == "AFINN", c("word", "score")]
#--------------------------------------------------------------------------
amazon_finefood<-read.csv("Amazonfoodreview.csv")
amazon_finefood<-amazon_finefood[,-c(1)]

reviews_source <- VectorSource(amazon_finefood$review_text)
corpus <- SimpleCorpus(reviews_source)
str(corpus[[1]])
corpus <- tm_map(corpus, content_transformer(tolower))
replaceCharacter <- content_transformer(function(x, pattern, replacement)
  gsub(pattern = pattern,replacement = replacement, x))
corpus <- tm_map(corpus, replaceCharacter, "'", "")
corpus <- tm_map(corpus, replaceCharacter, "[[:punct:]]", " ")
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)
saveRDS(corpus, "amazonfindfood_corpus.rds")
#----------------------------------------------------------------------------
str(corpus[[1]])
documents <- sapply(corpus, function(x) x)
head(documents, 2)
documents <- strsplit(documents, split = " ")
head(documents, 2)
# put the list structure into a data frame
documents <- melt(documents)
colnames(documents) <- c("word", "document")
# We can use merge to look up the sentiment for each word
documents[["word"]] <- as.character(documents[["word"]]) 
documents <- documents[documents$word != "", ]
documents <- merge(documents, lexicon, by = "word", all.x = TRUE)

# words for which no sentiment is available
sum(is.na(documents$score)) / nrow(documents)
sample(documents$word[is.na(documents$score)], 25)

# Calculate overall sentiment per review
sentiment_scores <- tapply(documents$score, factor(documents$document), 
                           function(x) sum(x, na.rm = TRUE) / max(1, sum(!is.na(x))) )
ratings <- amazon_finefood$rating
boxplot(sentiment_scores ~ ratings, data = data.frame(ratings, sentiment_scores), 
        xlab = "Star rating", ylab = "Sentiment score")

sentiment_categories<-ifelse(sentiment_scores>0,1,0)
sentiment_categories <- cut(sentiment_scores, breaks = quantile(sentiment_scores, seq(0,1/3)), 
                            labels = c("<4Stars", "5Stars"))
#---------------------------------------------------------------------------------------
binary_ratings <- amazon_finefood$binary_rating
confusionMatrix(sentiment_categories, binary_ratings)
