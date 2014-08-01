#' @rdname get.IDR
#' @return \code{get.idr} returns a vector where the i'th entry is the
#'   posterior probability that observation i is irreproducible.
#' @export
get.idr <- function (x, theta, ...) {
  #pseudo.data <- qgmm.marginal(u = Uhat(x), theta = theta, ...)
  #kappa <- EStep(x = pseudo.data, theta = theta)
  return(get.prob(Uhat(x), theta, ...)[,1])
}
