#' Classify observations
#'
#' Classify observations according to the maximum a posterior probabilites.
#'
#' @param x A \code{matrix} of A) observations where rows corresponds to
#'   features and columns to studies or B) class probabilities with
#' @param theta A list of parameters for the full model as described in
#'   \code{\link{rtheta}}. If \code{theta} is supplied, \code{x} are assumed to
#'   be observations (A). If \code{theta} is missing, \code{x} are assumed to be
#'   probabilites (B).
#'
#' @return A integer vector of class numbers with length equal to the number of
#'   rows in \code{x}.
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
    stopifnot(all(0 <= x & x <= 1))
    kappa <- x
  } else {
    kappa <- get.prob(x, theta)
  }
  map_class <- apply(kappa, 1, which.max)
  return(map_class)
}
