labeled <- train

train1 <- df$review_word2vec
write(train1,"text_data.txt") 
model=word2vec(train_file = "text_data.txt",output_file ="model1.bin",layer1_size = 300,min_count = 40,num_threads = 4,window = 10,sample = 0.001,binary=1)
bin_to_txt("model1.bin","model1text.txt") 


library(Rcpp)
library(RcppArmadillo)
library(tm)
library(randomForest)
library(rpart)
library(rword2vec)



sourceCpp("converter.cpp")
vocab=as.data.frame(read.table("model1text.txt",header = F,stringsAsFactors = F,skip=1))


colnames(vocab)[1]="word"

num_clusters=floor(nrow(vocab)/10)

vocab <- vocab[-8356,]

kmean_model= kmeans(vocab[,2:ncol(vocab)], centers = num_clusters, iter.max = 150)
write.csv(kmean_model$cluster, file="kmeans_clusters2.csv")
write.csv(kmean_model$centers, file="kmeans_centers2.csv")
kmean_model=NULL

clusters=read.csv("kmeans_clusters2.csv")
clusters$word=vocab$word
colnames(clusters)=c("word_no","cluster","word")

### bag of centroid
d=NULL
d=strsplit(labeled$review_word2vec," ")
for(i in 1:nrow(labeled))
{
  d[[i]]=gsub("^\\s+|\\s+$", "",d[[i]])
}

training_data= get_bag_of_centroids (d ,vocab$word,clusters$cluster,num_clusters)
training_data=as.data.frame(training_data)
training_data$rating=labeled$rating

write.csv(training_data,"train_bag_of_centroids.csv",row.names = F)

### test data

d=NULL
d=strsplit(test$review_word2vec," ")
for(i in 1:nrow(test))
{
  d[[i]]=gsub("^\\s+|\\s+$", "",d[[i]])
}
test_data=get_bag_of_centroids(d,vocab$word,clusters$cluster,num_clusters)
test_data=as.data.frame(test_data)
write.csv(test_data,"test_bag_of_centroids.csv",row.names = F)

training_data=read.csv("train_bag_of_centroids.csv")
test_data=read.csv("test_bag_of_centroids.csv")




feat <- training_data %>% rename(binary_rating = rating) 
# colnames(training_data)
# Make features a sparse matrix for the models
feat_sparse <- Matrix::sparse.model.matrix(data = feat,
                                           object = binary_rating ~ .-1)


mod_glmnet  <-  cv.glmnet(x = training_data[,1:835],
                          y = training_data$rating, 
                          family = 'binomial', 
                          alpha = 1,
                          type.measure = "auc",
                          nfolds = 5,
                          thresh = 1e-3,
                          maxit = 1e3)

# model
mod_xgboost <- xgboost(data = feat_sparse,
                       label = feat$binary_rating,
                       nrounds = 100,
                       objective = "binary:logistic")

# ============================= Predict on Test Set ===================================
pred_test <- data_frame(pred_glmnet = predict(mod_glmnet, dtm_test, type = 'response')[, 1],
                        pred_xgboost = predict(mod_xgboost, dtm_test, type = 'response'),
                        actual = test$binary_rating)











## randomForest
ml_model=randomForest(as.factor(rating)~.,data=training_data[1:(round(0.8*nrow(training_data))),],importance=T,ntree=50)
pred=predict(ml_model,training_data[(round(0.8*nrow(training_data))+1):nrow(training_data),])
tab <- table(pred,training_data$rating[(round(0.8*nrow(training_data))+1):nrow(training_data)])
pred_test=predict(ml_model,test_data)


submit=data.frame(id=test$id,predicted_rating=pred_test)

write.csv(submit,"rword2vec_boc_forest_solution.csv",row.names = F)