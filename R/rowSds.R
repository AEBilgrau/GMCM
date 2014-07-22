#' @rdname colSds
#' @return \code{rowSds} returns a numeric vector of length \code{n}.
#' @examples
#' y <- matrix(rnorm(50), 10, 5)
#' GMCM:::rowSds(y)
rowSds <- function(x) {
  n <- ncol(x)
  means <- rowMeans(x)
  return(sqrt(rowMeans((x - means)^2)*(n/(n - 1))))
}

