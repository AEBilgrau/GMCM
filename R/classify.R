#' Classify observations
#'
#' Classify observations according to the maximum a posterior probabilites.
#'
#' @inheritParams get.IDR
#'
#' @return
#' A integer vector of class numbers.
#'
#' @examples
#' set.seed(4821)
#' theta <- rtheta(d = 2, m = 3)
#' x <- matrix(runif(75), 25, 3)
#' classify(x, theta)
#' @seealso \code{\link{get.prob}}
#' @export
classify <- function(x, theta) {
  if (missing(theta)) {
    kappa <- x
  } else {
    kappa <- get.prob(x, theta)
  }
  map_class <- apply(kappa, 1, which.max)
  return(map_class)
}
