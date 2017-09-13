projectedmatrix = t(as.textmatrix(myMatrix.lsa))
projectedmatrix <-matrix(projectedmatrix, nrow = dim(projectedmatrix)[1],
                                ncol = dim(projectedmatrix)[2])


label <- cleaned_out$binary_rating
projectedmatrix <- as.data.table(projectedmatrix)
projectedmatrix$binary_rating <- label


feat_sparse <- Matrix::sparse.model.matrix(data = projectedmatrix,object = binary_rating ~ .-1)
ind <- sample(1:nrow(projectedmatrix), round(nrow(projectedmatrix)*0.8334), replace = FALSE)

train_feat <- feat_sparse[ind, ]
train_label <- projectedmatrix[ind, ]

test_feat <- feat_sparse[-ind, ]
test_label <- projectedmatrix[-ind, ]







mixed
auc(roc_obj_xgb)
Area under the curve: 0.8557
> auc(roc_obj_glm)
Area under the curve: 0.8463

blind
> auc(roc_obj_xgb)
Area under the curve: 0.8535
> auc(roc_obj_glm)
Area under the curve: 0.8444


if(!require("ROCR")) install.packages("ROCR"); library("ROCR")
if(!require("pROC")) install.packages("pROC"); library("pROC")
result <- result_blind5k1k 
roc_obj_xgb <- roc(result$actual,round(result$pred_xgboost, digits = 0))
roc_obj_glm <- roc(result$actual,round(result$pred_glmnet, digits = 0))