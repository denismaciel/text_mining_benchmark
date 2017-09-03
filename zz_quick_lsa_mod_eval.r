

reviews <- readRDS("./data/yelp_reviews.rds", refhook = NULL)

if(!require("tm")) install.packages("tm"); library("tm")
if(!require("lsa")) install.packages("lsa"); library("lsa")
if(!require("SciencesPo")) install.packages("SciencesPo"); library("SciencesPo")
if(!require("caret")) install.packages("caret"); library("caret")
if(!require("data.table")) install.packages("data.table"); library("data.table")
if(!require("ROCR")) install.packages("ROCR"); library("ROCR")
if(!require("pROC")) install.packages("pROC"); library("pROC")

#feature creation with small reviews only(nchar<140 characters)
#known_f_matr_s <- reviews[nchar(reviews$review_text, type = "chars", allowNA = FALSE, keepNA = NA)<145,]
#for now, we use only 1000 reviews
known_f_matr_s <- reviews#[sample(1:nrow(reviews), 1000,replace=FALSE),]
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
cr_feature$rating <- known_f_matr_s$binary_rating

#create test/train set
test_index <- createDataPartition(cr_feature$rating, p = .2, list = F)
Test<-cr_feature[test_index,]
Train<-cr_feature[-test_index,]

# create model

model<-glm(rating~., data=Train, family="binomial")
prediction<-predict(model, Test, type="response")


#evaluate prediction

prediction_table <- as.data.frame(prediction)
prediction_table$ID <- as.integer(rownames(prediction_table))
setDT(prediction_table)
setkey(prediction_table,ID)
known_f_matr_s$ID <- 1:nrow(known_f_matr_s)
setkey(known_f_matr_s,ID)
result <-merge(known_f_matr_s,prediction_table, by="ID")


pred <- prediction(round(result$prediction, digits = 0),result$binary_rating)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)

roc_obj <- roc(result$binary_rating,round(result$prediction, digits = 0))
detach(package:glmnet)
auc(roc_obj)

nrow(result[result$binary_rating==round(result$prediction, digits = 0),])
