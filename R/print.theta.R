#' Print method for theta class
#'
#' @param x A \code{theta} object. See \code{\link{rtheta}}.
#' @param ... Arguments to be passed to subsequent methods.
#' @return Invisibly returns \code{x}.
#' @author Anders Ellern Bilgrau <anders.ellern.bilgrau@@gmail.com>
#' @examples
#' theta <- rtheta()
#' print(theta)
#' @export
print.theta <- function(x, ...) {
  cat(sprintf("theta object with d = %d dimensions and m = %d components:\n\n",
              x$d, x$m))
  print(x[-(1:2)])
  return(invisible(x))
}
