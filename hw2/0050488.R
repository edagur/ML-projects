library(MASS)
require(MASS)
#reading the data into variables
image_data <- read.csv(file = "hw02_data_set_images.csv", header = FALSE)
label_data <- read.csv(file = "hw02_data_set_labels.csv", header = FALSE)

K <- 5
d <- 320
N_training <- 125
y_index = 0
epsilon <- 1e-3
set.seed(521)
oneMatrix <- matrix(1:1,nrow=125,ncol=5)
X <- matrix(ncol = 320, nrow = 0)
X_test <- matrix(ncol = 320, nrow = 0)
Y <- matrix(0:0, ncol = 5, nrow = 125)
Y_test <- matrix(0:0, ncol = 5, nrow = 70)

for(L in c('A', 'B','C','D','E')){  
  X<-rbind(X, image_data[which(label_data==L)[1:25],])
  X_test<-rbind(X_test,image_data[which(label_data==L)[26:39],])
  y_index <- y_index +1
  lower = (1+25*(y_index-1))
  upper =  (25*y_index)
  test_lower = (1+14*(y_index-1))
  test_upper =  (14*y_index)
  Y[lower:upper,y_index] =1
  Y_test[test_lower:test_upper,y_index] =1
}

sigmoid <- function(X, W, w0) {
  score <- as.matrix(cbind(X, 1)) %*% rbind(W, w0)
  return (1 / (1 + exp(-(score))))
}

gradient_W <- function(X,Y,Y_scoring, eta){
  gradient_W = (-sapply(X = 1:K, function(c) colSums(matrix((Y[,c] - Y_scoring[,c])*Y_scoring[,c]*(oneMatrix[,c]-Y_scoring[,c]), nrow = N_training, ncol = d, byrow = FALSE) * X)))
  return (eta * gradient_W)
}

gradient_w0 <- function(Y,Y_scoring, eta){
  gradient_w0 = (-colSums((Y-Y_scoring)*(Y_scoring)*(oneMatrix-Y_scoring)))
  return (eta*gradient_w0)
}

W <- matrix(runif(d * K, min = -0.01, max = 0.01), d, K)
w0 <- runif(K, min = -0.01, max = 0.01)
iteration <- 1
objective_values <- c()

while (1) {
  
  Y_scoring_training <- sigmoid(X, W, w0)
  Y_scoring_test <- sigmoid(X_test,W,w0)
  
  objective_values <- c(objective_values, 0.5*sum((Y_scoring_training-Y)^2))
  
  current_gradient_W =gradient_W(X, Y, Y_scoring_training, eta = 0.01)
  current_gradient_w0 = gradient_w0(Y, Y_scoring_training, eta = 0.01)
  
  W <- W - current_gradient_W
  w0 <- w0 - current_gradient_w0
  
  if ( sqrt(sum(current_gradient_W^2) + sum(current_gradient_w0^2)) < epsilon) {
    break
  }
  
  iteration <- iteration + 1
}

plot(1:iteration, objective_values,
     type = "l", lwd = 2, las = 1,
     xlab = "Iteration", ylab = "Error")

Y_predicted_training <- apply(Y_scoring_training, 1, which.max)
Y_real_training <- apply(Y, 1, which.max)
confusion_matrix_training <- as.matrix(as.data.frame.matrix(table(Y_predicted_training, Y_real_training)))

Y_predicted_test <- apply(Y_scoring_test, 1, which.max)
Y_real_test <- apply(Y_test, 1, which.max)
confusion_matrix_test <- as.matrix(as.data.frame.matrix(table(Y_predicted_test, Y_real_test)))

