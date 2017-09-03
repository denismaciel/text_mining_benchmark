


if(!require("tm")) install.packages("tm"); library("tm")
if(!require("lsa")) install.packages("lsa"); library("lsa")
if(!require("SciencesPo")) install.packages("SciencesPo"); library("SciencesPo")
if(!require("caret")) install.packages("caret"); library("caret")
if(!require("data.table")) install.packages("data.table"); library("data.table")


# ============================= Creating Features ===================================
reviews <- readRDS("./data/yelp_reviews.rds", refhook = NULL)
known_f_matr_s <- reviews
#create corpus
corpus <- Corpus(VectorSource(known_f_matr_s$review_text))
#remove stopwords
corpus <- tm_map(corpus, removeWords, stopwords("english"))
#create TermDocumentMatrix
myMatrix = TermDocumentMatrix(corpus)
myMatrix = removeSparseTerms(myMatrix,0.98)
myMatrix = as.matrix(myMatrix)

#create latent semantic space
myMatrix.lsa = lsa(myMatrix,dimcalc_share())
myMatrix.lsa_tk=as.data.frame(myMatrix.lsa$tk)
myMatrix.lsa_dk=as.data.frame(myMatrix.lsa$dk)
myMatrix.lsa_sk=as.data.frame(myMatrix.lsa$sk)

#create features from discovered latent concepts
cr_feature <- myMatrix.lsa_dk
cr_feature$binary_rating <- known_f_matr_s$binary_rating


saveRDS(cr_feature, file = "./data/LSA_features_yelp.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)