#' Coerce a list to a theta object
#'
#' First, the class is added. Next, matrix means and array covariances are
#' coerced to list form.
#' Covariances on array form are assumed to be \code{d} by \code{d} by \code{m}.
#' Means on matrix form are as assumed to be \code{d} by \code{m}. I.e.
#' rows correspond to the dimensions and colums to components, or the mean vectors
#' as column vectors.
#'
#' @param x A theta-like object that can be coerced.
#' @return A theta object. See \code{\link{rtheta}}.
#' @examples
#' m <- 2
#' d <- 3
#' x <- list(m = m,
#'           d = d,
#'           pie = c(0.5, 0.5),
#'           mu = list(comp1=rep(0,d), comp2=rep(1,d)),
#'           sigma = list(comp1=diag(d), comp2=diag(d)))
#' print(x)
#' theta <- as.theta(x)
#' print(theta)
#'
#' x2 <- list(m = m,
#'            d = d,
#'            pie = c(0.5, 0.5),
#'            mu = simplify2array(list(comp1=rep(0,d), comp2=rep(1,d))),
#'            sigma = simplify2array(list(comp1=diag(d), comp2=diag(d))))
#' theta2 <- as.theta(x2)
#' print(theta2)
#' @export
as.theta <- function(x) {
  class(x) <- "theta"

  # Convert 'matrix' means to list
  if (is.matrix(x[[4]]) && is.numeric(x[[4]])) {
    stopifnot(nrow(x[[4]]) == x[[2]], # d
              ncol(x[[4]]) == x[[1]]) # m
    x[[4]] <- structure(lapply(seq_len(x[[1]]), function(j) x[[4]][, j]),
                        names = colnames(x[[4]]))
  }

  # Convert higher order array covariances to list
  if (is.array(x[[5]]) && is.numeric(x[[5]])) {
    stopifnot(dim(x[[5]]) == c(x[[2]], x[[2]], x[[1]]))
    x[[5]] <- structure(lapply(seq_len(x[[1]]), function(k) x[[5]][, , k]),
                        names = dimnames(x[[5]])[[3]])
  }

  if (is.theta(x)) {
    return(x)
  } else {
    stop("Could not coerce 'x' into a theta object.")
  }
}
