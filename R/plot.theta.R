#' Plotting method for "theta" objects
#'
#' Visualizes the chosen dimensions of the theta object graphically by the GMM
#' density and possibly the individual gaussian components.
#'
#' @param x An object of class \code{theta}.
#' @param which.dims An integer vector of length 2 choosing which two dimensions
#'   to plot.
#' @param n.sd An integer choosing the number of standard deviations in each
#'   dimension to determine the plotting window.
#' @param add.means logical. If TRUE, dots corresponding to the means are
#'    added to the plot.
#' @param ... Arguments passed to \code{contour}.
#' @param add.ellipses logical. If TRUE, ellipses outlining a 95\% confidence
#'   regions for each component are added in the bivariate multivariate
#'   distribution defined by theta and \code{which.dims}.
#' @return Plots via the \code{contour} function. Invisibly returns a list with
#'   x, y, z coordinates that is passed to contour.
#' @author Anders Ellern Bilgrau <anders.ellern.bilgrau@@gmail.com>
#' @examples
#' set.seed(5)
#' theta <- rtheta(d = 3, m = 2)
#' \dontrun{
#' plot(theta)
#' plot(theta, col = "blue", asp = 1, add.means = FALSE)
#' plot(theta, col = "blue", asp = 1, add.means = TRUE)
#' plot(theta, which.dims = c(3L, 2L), asp = 1)
#' }
#' plot(theta, asp = 1, n.sd = 3, add.ellipses = TRUE,
#'      nlevels = 40, axes = FALSE,
#'      xlab = "Dimension 1", ylab = "Dimension 2")
#' @importFrom ellipse ellipse
#' @importFrom graphics points lines contour
#' @importFrom stats qnorm
#' @export
plot.theta <- function(x, which.dims = c(1L,2L), n.sd = qnorm(0.99),
                       add.means = TRUE, ..., add.ellipses = FALSE) {

  # plot(xx$mu$comp1)
  stopifnot(is.theta(x))
  stopifnot(is.integer(which.dims))
  stopifnot(length(which.dims) == 2)
  stopifnot(all(which.dims <= x$d))

  # Subset theta to relevant dimensions
  new_theta <- x
  new_theta$d <- 2
  new_theta$mu <- lapply(x$mu, "[", which.dims)
  new_theta$sigma <- lapply(x$sigma, function(m) m[which.dims, which.dims])

  # Wrapper for GMM log-likelihood
  l <- 1000

  # Identify plotting area
  dim_means <- do.call("cbind", new_theta$mu)
  dim_sds <- sqrt(sapply(new_theta$sigma, diag))

  dim_min <- apply(dim_means - n.sd*dim_sds, 1, min)
  dim_max <- apply(dim_means + n.sd*dim_sds, 1, max)

  # Create grid to evaluate loglikelihood on
  x2 <- seq(dim_min[1], dim_max[1], length.out = l)
  y2 <- seq(dim_min[2], dim_max[2], length.out = l)

  # Evauate loglikelihood
  res2 <- outer(x2, y2, FUN = function(x, y) {
    dgmm.loglik(new_theta, z = cbind(x, y), marginal.loglik = TRUE)
  })

  # Capture ...
  additional.args <- list(...)
  if (!("col" %in% names(additional.args))) {
    additional.args$col <- "#FF000080"
  }
  if (!("xlab" %in% names(additional.args))) {
    additional.args$xlab <- paste("Dim", which.dims[1])
  }
  if (!("ylab" %in% names(additional.args))) {
    additional.args$ylab <- paste("Dim", which.dims[2])
  }

  # Perform the plotting operations
  do.call("contour", c(list(x = x2, y = y2, z = res2),
                                 additional.args))

  if (add.means) {
    graphics::points(do.call("rbind", new_theta$mu), pch = 16, cex = .6)
  }

  if (add.ellipses) {

    for (comp in seq_len(new_theta$m)) {
      ell <- ellipse::ellipse(x = new_theta$sigma[[comp]],
                              which = 1:2, # new_theta is already subsetted
                              centre = new_theta$mu[[comp]])
      graphics::lines(ell, lwd = 2, col = "red")
    }
  }

  return(invisible(list(x = x2, y = y2, z = res2)))
}
