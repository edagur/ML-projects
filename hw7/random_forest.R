library(AUC)
library(randomForestSRC)
X_train <- read.csv("training_data.csv", header = TRUE)
X_test <- read.csv("test_data.csv", header = TRUE)
y_train <- as.factor(read.csv("training_labels.csv", header = FALSE)[,1])

positive_indices = which(as.numeric(y_train)==2)
negative_indices = which(as.numeric(y_train)==1)
ntree_set <- c(1:5) * 100
nfold = 5
auroc_matrix <- matrix(NA, nrow = nfold, ncol = length(ntree_set),0)

for(fold in 1:nfold){
  positive_X = X_train[positive_indices,]
  negative_X = X_train[negative_indices,]
  validation_positive = positive_X[(length(positive_indices)*((fold-1)/nfold)+1):(length(positive_indices)*(fold/nfold)), ]
  validation_negative = negative_X[(length(negative_indices)*((fold-1)/nfold)+1):(length(negative_indices)*(fold/nfold)), ]
  validation = rbind(validation_positive, validation_negative)
  validation_label = rbind(matrix(ncol=1, nrow = nrow(validation_positive),2),matrix(ncol=1, nrow = nrow(validation_negative),1))
  
  
  training_positive = positive_X[-c((length(positive_indices)*((fold-1)/nfold)+1):(length(positive_indices)*(fold/nfold))),]
  training_negative = negative_X[-c((length(negative_indices)*((fold-1)/nfold)+1):(length(negative_indices)*(fold/nfold))),]
  training = rbind(training_positive, training_negative)
  training_label = rbind(matrix(ncol=1, nrow = nrow(training_positive),2),matrix(ncol=1, nrow = nrow(training_negative),1))
  
  for(ntree in ntree_set){
    state <- rfsrc(formula = y ~ ., data = cbind(training, y = as.factor(training_label)), ntree = 500)
    
    prediction <- predict(state, newdata = validation)
    roc_curve <- roc(predictions = prediction$predicted[,2], labels = as.factor(validation_label))
    print(sprintf("%g", auc(roc_curve)))
    auroc_matrix[fold, which(ntree_set==ntree)] = auc(roc_curve)
  }
  
}
ntree_star_AUROC <- 500
ntree_star_AUROC <- ntree_set[which.max(colMeans(auroc_matrix, na.rm = TRUE))]

state <- rfsrc(formula = y ~ ., data = cbind(X_train, y = y_train), ntree = ntree_star_AUROC)
training_prediction <- predict(state, newdata = X_train)
roc_curve <- roc(predictions = training_prediction$predicted[,2], labels = y_train)
plot(roc_curve$fpr, roc_curve$tpr, lwd = 2, col = "blue", type = "b", las = 1)
auc(roc_curve)

test_prediction <- predict(state, newdata = X_test)
write.table(test_prediction$predicted[,2], file = "test_predictions.csv", row.names = FALSE, col.names = FALSE)
