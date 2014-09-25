#' @name colSds
#' @aliases rowSds colSds
#' @title Compute row and column standard deviations
#' @description The \code{rowSds} and \code{colSds} respectively computes the
#'   standard deviations of the rows and columns of the given matrix.
#' @param x A numeric matrix of size \code{n} times \code{m}
#' @return \code{colSds} returns a numeric vector of length \code{m}.
#' @author Anders Ellern Bilgrau <abilgrau (at) math.aau.dk>
#' @seealso \code{\link{rowMeans}}, \code{\link{colMeans}}
#' @examples
#' x <- matrix(rnorm(50), 10, 5)
#' GMCM:::colSds(x)
#' @keywords internal
colSds <- function(x) {
  ans <- colSdsArma(x)
  dim(ans) <- NULL
  names(ans) <- colnames(x)
  return(ans)
}

