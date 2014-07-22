#' Compute row and column standard deviations
#' 
#' The \code{rowSds} and \code{colSds} respectively computes the 
#' standard deviations of the rows and columns of the given matrix.
#' @param x A numeric matrix of size \code{n} times \code{m}
#' @return \code{rowSds} returns a numeric vector of length \code{n}.
#' @author Anders Ellern Bilgrau <abilgra''umath.aau.dk>
#' @seealso \code{\link{rowMeans}}, \code{\link{colMeans}}
#' @examples
#' x <- matrix(rnorm(50), 10, 5)
#' rowSds(x)
colSds <- function(x) {
  n <- nrow(x)
  means <- colMeans(x)
  return(sqrt(colMeans((x-means)^2)*(n/(n-1))))
}