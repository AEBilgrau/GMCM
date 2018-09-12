#' Summary method for theta class
#'
#' @param x A \code{theta} object. See \code{\link{rtheta}}.
#' @return Invisibly returns \code{x}.
#' @author Anders Ellern Bilgrau <anders.ellern.bilgrau@@gmail.com>
#' @examples
#' theta <- rtheta()
#' summary(theta)
#' @export
summary.theta <- function(x) {
  cat(sprintf("A theta object with d = %d dimensions and m = %d components.\n",
              x$d, x$m))
  return(invisible(x))
}
