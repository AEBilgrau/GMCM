#' Summary method for theta class
#'
#' @param object A \code{theta} object. See \code{\link{rtheta}}.
#' @param ... arguments to be passed to subsequent methods
#' @return Invisibly returns \code{x}.
#' @author Anders Ellern Bilgrau <anders.ellern.bilgrau@@gmail.com>
#' @examples
#' theta <- rtheta()
#' summary(theta)
#' @export
summary.theta <- function(object, ...) {
  cat(sprintf("A theta object with d = %d dimensions and m = %d components.\n",
              object$d, object$m))
  return(invisible(object))
}
