#projected.train = fold_in(docvecs = as.matrix(tdm_train), LSAspace = myMatrix.lsa)

projected.train.matrix = t(as.textmatrix(myMatrix.lsa))
projected.train.matrix <-matrix(projected.train.matrix, nrow = dim(projected.train.matrix)[1],
                                ncol = dim(projected.train.matrix)[2])


train_label <- cr_feature$binary_rating
projected.train.matrix <- as.data.table(projected.train.matrix)
projected.train.matrix$binary_rating <- train_label


projected.test = fold_in(docvecs = as.matrix(tdm), LSAspace = myMatrix.lsa)

projected.test.matrix <- t(projected.test)
projected.test.matrix <-matrix(projected.test.matrix, nrow = dim(projected.test.matrix)[1],
                                ncol = dim(projected.test.matrix)[2])
test_label <- cleaned_out$binary_rating
projected.test.matrix <- as.data.table(projected.test.matrix)
projected.test.matrix$binary_rating <- test_label

feat <- rbind(projected.train.matrix,projected.test.matrix)
feat_sparse <- Matrix::sparse.model.matrix(data = feat,object = binary_rating ~ .-1)
train_feat <- feat_sparse[1:nrow(projected.train.matrix),]
test_feat <- feat_sparse[(nrow(projected.train.matrix)+1):nrow(feat_sparse),]

library(xgboost)
library(glmnet)
# ============================= Train the Model ===================================
mod_glmnet  <-  cv.glmnet(x = train_feat,
                          y = train_label, 
                          family = 'binomial', 
                          alpha = 1,
                          type.measure = "auc",
                          nfolds = 5,
                          thresh = 1e-3,
                          maxit = 1e3)

mod_xgboost <- xgboost(data = train_feat,
                       label = train_label,
                       nrounds = 100,
                       objective = "binary:logistic")

# ============================= Predict on Test Set ===================================
pred_test <- data_frame(pred_glmnet = predict(mod_glmnet, test_feat, type = 'response')[, 1],
                        pred_xgboost = predict(mod_xgboost, test_feat, type = 'response'),
                        actual = test_label)







if(!require("ROCR")) install.packages("ROCR"); library("ROCR")
if(!require("pROC")) install.packages("pROC"); library("pROC")
result <- as.data.frame(pred_test)
roc_obj_xgb <- roc(result$actual,round(result$pred_xgboost, digits = 0))
roc_obj_glm <- roc(result$actual,round(result$pred_glmnet, digits = 0))
#detach(package:glmnet)
auc(roc_obj_xgb)
auc(roc_obj_glm)