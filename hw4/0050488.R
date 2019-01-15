# read data into memory
data_set <- read.csv("hw04_data_set.csv")

# get x and y values
x_train <- data_set$x[1:100]
x_test <- data_set$x[-(1:100)]
y_train <- data_set$y[1:100]
y_test <- data_set$y[-(1:100)]

min_value <-0
max_value <- ceiling(max(data_set$x)/3) * 3
data_interval <- seq(from = min_value, to = max_value, by = 0.01)

plot_points_and_legends <- function (bin_width){
  plot(x_train, y_train,pch = 19, col = "blue",  xlim = c(min_value, max_value), ylab = "y", xlab = "x", main = paste("h = ", bin_width))
  points(x_test, y_test, pch = 19, col= "red")
  legend(52.5, 83, legend=c("training", "test"), col=c("blue", "red"), pch=19, cex=0.8)
}

plot_lines <- function(p_head, interval){
  for (b in 1:length(interval)) {
    lines(c(interval[b], interval[b+1]), c(p_head[b], p_head[b]), lwd = 2, col = "black")
    lines(c(interval[b+1], interval[b+1]), c(p_head[b], p_head[b + 1]), lwd = 2, col = "black") 
  }
}

RMSE<- function(bin_width, p_head_list, interval_width){
y_estimated <- sapply(1:length(y_test), function(i) {
  interval_number <- ceiling((x_test[i]-min_value) / interval_width)
  return (p_head_list[interval_number])
})
error_squares <- sapply(1:length(y_test), function(i) {
  diff <- y_test[i] - y_estimated[i]
  return(diff^2)
})
return (sqrt(mean(error_squares)))
}

# regrossogram estimator
bin_width <- 3
borders <- seq(from = min_value, to = max_value, by = bin_width)
p_head <- sapply(1:length(borders), function(b) {
  bin = y_train[borders[b] < x_train & x_train <= borders[b+1]]
  return(mean(bin))
})
plot_points_and_legends(bin_width)
plot_lines(p_head, borders)
RMSE_regrossogram <- RMSE(bin_width, p_head, bin_width)
sprintf("Regressogram => RMSE is %.4f when h is %s", RMSE_regrossogram, bin_width)

#running mean smoother
bin_width <- 3
p_head <- sapply(data_interval, function(x) {
  bin <- y_train[(x - 0.5 * bin_width) < x_train & x_train <= (x + 0.5 * bin_width)]
  return(mean(bin))
})
plot_points_and_legends(bin_width)
plot_lines(p_head, data_interval)
RMSE_running_mean_smoother <- RMSE(bin_width, p_head, 0.01)
sprintf("Running mean smoother => RMSE is %s when h is %s", RMSE_running_mean_smoother, bin_width)

#kernel smoother
bin_width <- 1
gaussian_kernel = function(interval, training_point) {
  u <- (interval - training_point) / bin_width
  (1 / sqrt((2 * pi))) * exp(-0.5*u^2)
}
p_head <- sapply(data_interval, function(x) {
  kernel_list <- sapply(1:length(x_train), function(i) {
    return(gaussian_kernel(x, x_train[i]))
  })
  kernel_times_y_list <- sapply(1:length(kernel_list), function(i) {
    return(kernel_list[i]*y_train[i])
  })
  return(sum(kernel_times_y_list) / sum(kernel_list))
})
plot_points_and_legends(bin_width)
plot_lines(p_head, data_interval)
RMSE_kernel_smoother <- RMSE(bin_width, p_head, 0.01)
sprintf("Kernel smoother => RMSE is %s when h is %s", RMSE_kernel_smoother, bin_width)