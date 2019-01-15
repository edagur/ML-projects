library(MASS)
library(mixtools)
set.seed(521)
#Q1
given_means <- matrix(c(2.5, 2.5,-2.5, 2.5,-2.5, -2.5,2.5, -2.5,0.0,  0.0),5,2, byrow = TRUE)
given_covs <- matrix(c(0.8, -0.6, -0.6, 0.8,0.8, 0.6, 0.6, 0.8,0.8, -0.6, -0.6, 0.8,
                       0.8, 0.6, 0.6, 0.8, 1.6,  0.0,  0.0, 1.6),5,4, byrow = TRUE)
class_sizes <- c(50,50,50,50,100)
colors = c("blue"," green", "red", "orange", "purple")
X<-matrix(nrow=0, ncol=2, 0)
N <- sum(class_sizes)
K <- 5
for(k in 1:K){
  X<-rbind(X,mvrnorm(n = class_sizes[k], mu = given_means[k,], Sigma = matrix(c(given_covs[k,]), 2,2, byrow = TRUE)))
}
plot(X[,1], X[,2], type = "p", pch = 19, xlim = c(-6, 6), ylim = c(-6, 6), xlab = "x1", ylab = "x2")

#Initial Centroids
centroids <- X[sample(1:N, K),]

D<-matrix(0,nrow = K, ncol = N)
for (i in 1:N) {
  for (c in 1:K) {
    D[c,i] <- dist(rbind(X[i,], centroids[c]), method = "euclidean")
  }}
assignments <- sapply(1:ncol(D), function(i) {which.min(D[,i])})
for (k in 1:K) {
  centroids[k,] <- colMeans(X[assignments == k,])
} 

D<-matrix(0,nrow = K, ncol = N)
for (i in 1:N) {
  for (c in 1:K) {
    D[c,i] <- dist(rbind(X[i,], centroids[c,]), method = "euclidean")
  }}
assignments <- sapply(1:ncol(D), function(i) {which.min(D[,i])})
for (k in 1:K) {
  centroids[k,] <- colMeans(X[assignments == k,])
} 

prior <- sapply(1:K, function (c){ mean(assignments == c)}) 
covariances <- t(sapply(1:K, function (c) {cov(X[assignments == c,])}))
means <- centroids

for (rep in 1:100) {
  gaus <- matrix(nrow=N, ncol=5)
  for (i in 1:N) {
    for (c in 1:K) {
      gaus[i,c] <- prior[c] * ((det(matrix(covariances[c,],2,2)))^(-0.5)) *
        exp((-0.5)* t((X[i,]- means[c,])) %*% (solve(matrix(covariances[c,],2,2)))%*%(X[i,]- means[c,]))
    }
  }
  
  hic <- matrix(0, nrow = N, ncol = 5)
  for(i in 1:N){
    for(c in 1:K){
      hic[i,c] <-gaus[i,c] / rowSums(gaus)[i]
    }
  }
  prior <- colSums(hic)/N
  means <- (t(hic)%*%X)/colSums(hic)
  for (c in 1:K) {
    sum <- matrix(c(0,0,0,0),2,2)
    for (i in 1:N) {
      sum <- sum + hic[i,c]* (X[i, ] - means[c,]) %*% t((X[i, ] - means[c,]))
    }
    covariances[c, ] <- sum / colSums(hic)[c]
  }
}

D<-matrix(0,nrow = K, ncol = N)
for (i in 1:N) {
  for (c in 1:K) {
    D[c,i] <- dist(rbind(X[i,], means[c,]), method = "euclidean")
  }}
assignments <- sapply(1:ncol(D), function(i) {which.min(D[,i])})

plot(X[assignments == 1, 1], X[assignments == 1, 2], type = "p", pch = 19, col = "blue", xlim = c(-6, 6), ylim = c(-6, 6), xlab = "x1", ylab = "x2")
for(k in 2:K){
  points(X[assignments == k, 1], X[assignments == k, 2], type = "p", pch = 19, col = colors[k])
}

for(k in 1:K){
  ellipse(given_means[k,], matrix(c(given_covs[k,]), 2,2, byrow = TRUE), alpha = .05, npoints = class_sizes[k], newplot = FALSE, draw = TRUE, lty=2, lwd=2)
  ellipse(means[k,], matrix(covariances[k,], 2,2), alpha = .05, npoints = class_sizes[k], newplot = FALSE, draw = TRUE, lwd=2)
}