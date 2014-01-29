colSds <- function(x) {
  n <- nrow(x)
  means <- colMeans(x)
  return(sqrt(colMeans((x-means)^2)*(n/(n-1))))
}