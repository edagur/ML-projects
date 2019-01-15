#Q1
data_set <- read.csv("hw05_data_set.csv")
# get x and y values
X_train <- as.matrix(data_set$x[1:100])
X_test <- as.matrix(data_set$x[-(1:100)])
y_train <- data_set$y[1:100]
y_test <- data_set$y[-(1:100)]
# get numbers of train and test samples
N_train <- length(y_train)
N_test <- length(y_test)

#Q2
DecisionTree <- function(P) {
  # put all training instances into the root node
  node_indices <- list(1:N_train)
  is_terminal <- c(FALSE)
  need_split <- c(TRUE)
  node_splits <- c()
  node_means <- c()
  
  # learning algorithm
  while (1) {
    # find nodes that need splitting
    split_nodes <- which(need_split)
    # check whether we reach all terminal nodes
    if (length(split_nodes) == 0) {
      break
    }
    # find best split positions for all nodes
    for (split_node in split_nodes) {
      data_indices <- node_indices[[split_node]]
      need_split[split_node] <- FALSE
      node_mean <- mean(y_train[data_indices])
      unique_values <- sort(unique(X_train[data_indices]))
      if (length(X_train[data_indices]) <= P || length(unique_values) == 1) {
        is_terminal[split_node] <- TRUE
        node_means[split_node] <- node_mean
      } else {
        is_terminal[split_node] <- FALSE
        split_positions <- (unique_values[-1] + unique_values[-length(unique_values)]) / 2
        split_scores <- rep(0, length(split_positions))
        for (s in 1:length(split_positions)) {
          left_indices <- data_indices[which(X_train[data_indices] <= split_positions[s])]
          right_indices <- data_indices[which(X_train[data_indices] > split_positions[s])]
          right_score = sum((y_train[left_indices] - mean(y_train[left_indices], na.rm = TRUE)) ^ 2)
          left_score = sum((y_train[right_indices] - mean(y_train[right_indices], na.rm = TRUE)) ^ 2)
          split_scores[s] <- (right_score + left_score) / length(data_indices)
        }

        best_score <- which.min(split_scores)
        best_split <- split_positions[best_score]
        node_splits[split_node] <- best_split
        
        # create left node using the selected split
        left_indices <- data_indices[which(X_train[data_indices] < best_split)]
        node_indices[[2 * split_node]] <- left_indices
        is_terminal[2 * split_node] <- FALSE
        need_split[2 * split_node] <- TRUE
        
        # create right node using the selected split
        right_indices <- data_indices[which(X_train[data_indices] >= best_split)]
        node_indices[[2 * split_node + 1]] <- right_indices
        is_terminal[2 * split_node + 1] <- FALSE
        need_split[2 * split_node + 1] <- TRUE
      }
    }
  }
  y_predicted <- rep(0, N_test)
  for (i in 1:N_test) {
    index <- 1
    while (1) {
      if (is_terminal[index] == TRUE) {
        y_predicted[i] <- node_means[index]
        break
      } else {
        if (X_test[i] <= node_splits[index]) {
          index <- index * 2
        } else {
          index <- index * 2 + 1
        }
      }
    }
  }
  RMSE <- sqrt(mean((y_test - y_predicted) ^ 2))
  message(sprintf("RMSE is %.4f when P is %s", RMSE, P))
  result <- list(node_splits,node_means, is_terminal, RMSE)
  return(result)
}

#Q3(3)
result <- DecisionTree(10)
#Q4
plot(X_train, y_train, type = "p", main = "P=10", pch = 19, col = "blue",xlim = c(min(data_set$x)-1, max(data_set$x)+1.8),ylab = "y", xlab = "x", las = 1)
points(X_test, y_test, type = "p", pch = 19, col= "red")
legend(55,85, legend=c("training", "test"),col=c("blue", "red"), pch = 19, cex = 0.5, bty = "y")
data_interval <- seq(from = min(data_set$x)-1, to = max(data_set$x)+1.8, by = 0.01)
is_terminal = unlist(result[3])
node_means = unlist(result[2])
node_splits = unlist(result[1])
for (b in 1:length(data_interval)) {
  line_x <- c(data_interval[b], data_interval[b+1])
  line_y_predicted<-rep(0,2)
  for (i in 1:2) {
    index <- 1
    while (1) {
      if (is_terminal[index] == TRUE) {
        line_y_predicted[i] <- node_means[index]
        break
      } else {
        if (b!=length(data_interval) && line_x[i] <= node_splits[index]) {
          index <- index * 2
        } else {
          index <- index * 2 + 1
    }}}}
  lines(c(line_x[1], line_x[2]), c(line_y_predicted[1], line_y_predicted[1]), lwd = 2, col = "black")
  if (b < length(data_interval)) {
    lines(c(line_x[2], line_x[2]), c(line_y_predicted[1], line_y_predicted[2]), lwd = 2, col = "black") 
  }
}

#Q5
plot(1:20,lapply(X=1:20, FUN = function(P) {result <- unlist(DecisionTree(P)[4])} ),
type = "o", lwd = 1, las = 1, pch = 1, lty = 2, xlab = "P", ylab = "RMSE")
