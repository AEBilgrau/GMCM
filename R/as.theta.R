#' Coerce a list to a theta object
#'
#' @description At the moment, only the class is added.
#'
#' @param x A theta-like object that can be coerced.
#' @return A theta object. See \code{\link{rtheta}}.
#' @examples
#' m <- 2
#' d <- 2
#' x <- list(m = m,
#'           d = d,
#'           pie = c(0.5, 0.5),
#'           mu = list(comp1=rep(0,d), comp2=rep(1,d)),
#'           sigma = list(comp1=diag(d), comp2=diag(d)))
#' print(x)
#' theta <- as.theta(x)
#' print(theta)
#' @export
as.theta <- function(x) {
  class(x) <- "theta"
  if (is.theta(x)) {
    return(x)
  } else {
    stop("Could not coerce 'x' into a theta object.")
  }
}
