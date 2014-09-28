#' @rdname get.IDR
#' @return \code{get.idr} returns a vector where the i'th entry is the
#'   posterior probability that observation i is irreproducible. A simple wrapper
#'   for \code{get.prob}.
get.idr <- function (x, theta, ...) {
  return(get.prob(Uhat(x), theta, ...)[, 1])
}
