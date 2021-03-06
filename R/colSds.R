#' @name colSds
#' @aliases rowsds colsds
#' @title Row and column standard deviations
#' @description The \code{rowSds} and \code{colSds} respectively computes the
#'   standard deviations of each rows and columns of the given matrix.
#' @param x A numeric matrix of size \code{n} times \code{m}
#' @return \code{colSds} returns a numeric vector of length \code{m}.
#' @author Anders Ellern Bilgrau <anders.ellern.bilgrau@@gmail.com>
#' @seealso \code{\link{rowMeans}}, \code{\link{colMeans}}
#' @examples
#' x <- matrix(rnorm(50), 10, 5)
#' colSds(x)
#' apply(x, 2, sd)  # slower equivalent code
#' @export
colSds <- function(x) {
  stopifnot(nrow(x)>0)
  ans <- colSdsArma(x)
  dim(ans) <- NULL
  names(ans) <- colnames(x)
  return(ans)
}
