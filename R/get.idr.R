#' @name get.IDR
#' @usage NULL
#' @note
#'   From \pkg{GMCM} version 1.1 \code{get.IDR} has been an internal function.
#'   Use \code{get.prop} or \code{get.IDR} instead. The function can still be
#'   accessed with \code{GMCM:::get.idr}. \code{get.idr} returns a vector where
#'   the \eqn{i}'th entry is the posterior probability that observation \eqn{i}
#'   is irreproducible. It is a simple wrapper for \code{get.prob}.
get.idr <- function(x, theta, ...) {
  return(get.prob(Uhat(x), theta, ...)[, 1])
}
