#' Classify observations
#'
#' Classify observations according to the maximum a posterior probabilites.
#'
#' @param x Either a \code{matrix} of A) observations where rows corresponds to
#'   obsercations and columns to dimensions or B) class probabilities where rows
#'   correspond to obsevations and columns to components.
#' @param theta A list of parameters for the full model as described in
#'   \code{\link{rtheta}}. If \code{theta} is supplied, \code{x} are assumed to
#'   be observations (A). If \code{theta} is missing, \code{x} are assumed to be
#'   probabilites (B).
#'
#' @return A integer vector of class numbers with length equal to the number of
#'   rows in \code{x}.
#'
#' @examples
#' # Classify using probabilites (usually returned from get.prob)
#' probs <- matrix(runif(75), 25, 3)
#' classify(probs)
#'
#' # Classify using a matrix of observations and theta
#' theta <- rtheta(d = 4, m = 3)
#' u <- SimulateGMCMData(n = 20, theta = theta)$u
#' classify(x = u, theta = theta)
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
