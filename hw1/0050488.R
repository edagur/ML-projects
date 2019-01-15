safelog <- function(x) {
  return (log(x + 1e-100))
}
training_data <- matrix(ncol = 320, nrow = 0)
test_data <- matrix(ncol = 320, nrow = 0)
pcd <- matrix(ncol=320, nrow = 0 )
image_data <- read.csv(file = "hw01_data_set_images.csv", header=FALSE)
label_data <- read.csv(file = "hw01_data_set_labels.csv", header=FALSE)
#dividing the data into training and test, finding the estimated parameters and placing them in a matrix
for(L in c('A', 'B','C','D','E')){  
  training_data<-rbind(training_data, image_data[which(label_data==L)[1:25],])
  pcd <- rbind(pcd, colMeans(image_data[which(label_data==L)[1:25],])) 
  test_data<-rbind(test_data,image_data[which(label_data==L)[26:39],])
}
#scoring function for a single data class is achieved by parametric classification rule
scoring_function<-function( pcd_class, data_matrix){
  one_matrix=matrix(1:1, nrow = nrow(data_matrix), ncol = 320, byrow = TRUE)
  extended_pcd <- matrix(pcd_class, nrow = nrow(data_matrix), ncol = 320, byrow = TRUE) 
  prior_prob<- safelog(25/nrow(training_data))
  return (rowSums(data_matrix*safelog(extended_pcd) + (one_matrix-data_matrix)*safelog(one_matrix-extended_pcd))+prior_prob) 
}
#put all the scoring function vectors into a single matrix
scoring_function_matrix<-function(nclass, pcd=pcd, data_matrix){
  scoring_matrix=matrix(nrow=0, ncol = nrow(data_matrix))
  for(i in 1:nclass){
    scoring_matrix<-rbind(scoring_matrix, scoring_function( pcd[i,],data_matrix))
  }
  return(t(scoring_matrix))
}
#confusion matrix shows how we classified the elements compared to the actual classes
confusion<-function(nclass, class_size, scoring_matrix){
  confusion_matrix <- matrix(0:0,nclass, nclass)
  for (i in 1:nclass){
    for(j in 1:class_size){
     confusion_matrix[which.max(scoring_matrix[((i-1)*class_size+j),]),i] <- 1 +   confusion_matrix[which.max(scoring_matrix[((i-1)*class_size+j),]),i]
    }
  }
  return(confusion_matrix)
}
training_scoring_matrix <- scoring_function_matrix(5, pcd = pcd,training_data)
test_scoring_matrix <- scoring_function_matrix(5, pcd=pcd, test_data)
confusion_training <- confusion(5,25, training_scoring_matrix)
confusion_test <- confusion(5,14, test_scoring_matrix)