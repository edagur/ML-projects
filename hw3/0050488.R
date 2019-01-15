safelog <- function(x) {
  return (log(x + 1e-100))
}

set.seed(521)
N <- 125
D <- 320
K <- 5
H <- 20
y_index <- 0
eta <- 0.005 
epsilon <- 1e-3
iteration <- 1
max_iteration <- 200

X <- matrix(ncol = D, nrow = 0)
X_test <- matrix(ncol = D, nrow = 0)

y_boolean_matrix <- matrix(0:0, ncol = K, nrow = N)
y_truth = c(rep(1,25), rep(2,25),rep(3,25), rep(4,25), rep(5,25) )
y_test_truth = c(rep(1,14), rep(2,14),rep(3,14), rep(4,14), rep(5,14) )

image_data <- as.matrix(read.csv(file = "hw03_data_set_images.csv", header = FALSE))
label_data <- as.matrix(read.csv(file = "hw03_data_set_labels.csv", header = FALSE))

for(L in c('A', 'B','C','D','E')){  
  y_index <- y_index +1
  X<-rbind(X, image_data[which(label_data==L)[1:25],])
  X_test<-rbind(X_test,image_data[which(label_data==L)[26:39],])
  y_boolean_matrix[(1+25*(y_index-1)): (25*y_index),y_index] =1
}

sigmoid <- function(a) {
  return (1 / (1 + exp(-a)))
}

softmax <- function(Z, v) {
  output <- exp(cbind(1, Z) %*% v)
  softmax_value <- t(sapply(1:nrow(output),function(i)  output[i,] / sum(output[i,]) ))
  return(softmax_value)
}

W <- matrix(runif((D + 1) * H, min = -0.01, max = 0.01), D + 1, H)
v <- matrix(runif((H + 1) * K, min = -0.01, max = 0.01), H + 1, K)

Z <- sigmoid(cbind(1, X) %*% W)
y_predicted <- softmax(Z,v)
objective_values <- -sum(y_boolean_matrix * safelog(y_predicted))

while (1) {
  for (i in sample(N)) {
    Z[i,] <- sigmoid(c(1, X[i,]) %*% W)
    y_predicted[i,] <- softmax(matrix(Z[i,], 1, H), v)
    v <- v + sapply(1:K, function(k) eta * (y_boolean_matrix[i, k] - y_predicted[i, k]) * c(1, Z[i,]) )
    W <- W + sapply(1:H, function(h) eta * sum((y_boolean_matrix[i,] - y_predicted[i,]) * v[h+1,]) * Z[i, h] * (1 - Z[i, h]) * c(1, X[i,])
    )
  }
  Z <- sigmoid(cbind(1, X) %*% W)
  Z_test <- sigmoid(cbind(1, X_test) %*% W)
  y_predicted <- softmax(Z, v)
  y_test_predicted <- softmax(Z_test, v)
  
  objective_values <- c(objective_values, -sum(y_boolean_matrix * safelog(y_predicted)))
  
  if (abs(objective_values[iteration + 1] - objective_values[iteration]) < epsilon | iteration >= max_iteration) {
    break
  }
  iteration <- iteration + 1
}

plot(1:(iteration + 1), objective_values,
     type = "l", lwd = 2, las = 1,
     xlab = "Iteration", ylab = "Error")

y_predictions <-  sapply(1:nrow(y_predicted),function(i) which.max(y_predicted[i,]) )
confusion_matrix <- table(y_predictions, y_truth)
print(confusion_matrix)

y_test_predictions <-  sapply(1:nrow(y_test_predicted),function(i) which.max(y_test_predicted[i,]) )
confusion_matrix_2 <- table(y_test_predictions, y_test_truth)
print(confusion_matrix_2)