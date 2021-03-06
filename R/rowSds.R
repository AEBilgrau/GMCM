#' @rdname colSds
#' @return \code{rowSds} returns a numeric vector of length \code{n}.
#' @examples
#' y <- matrix(rnorm(50), 10, 5)
#' rowSds(y)
#' @export
rowSds <- function(x) {
  stopifnot(ncol(x)>0)
  ans <- rowSdsArma(x)
  dim(ans) <- NULL
  names(ans) <- rownames(x)
  return(ans)
}

