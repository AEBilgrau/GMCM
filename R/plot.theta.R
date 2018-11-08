#' Plotting method for "theta" objects
#'
#' Visualizes the chosen dimensions of the theta object graphically by the GMM
#' density and possibly the individual gaussian components.
#'
#' @param theta An object of class \code{theta}.
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
#' theta <- rtheta(d = 3, m = 4)
#' plot(theta)
#' plot(theta, col = "blue", asp = 1, add.means = FALSE)
#' plot(theta, col = "blue", asp = 1, add.means = TRUE)
#' plot(theta, which.dims = c(3L, 2L), asp = 1)
#' plot(theta, asp = 1, n.sd = 3, add.ellipses = TRUE,
#'      nlevels = 40, axes = FALSE)
#' @importFrom ellipse ellipse
#' @export
plot.theta <- function(theta, which.dims = c(1L,2L), n.sd = 2,
                       add.means = TRUE, ..., add.ellipses = FALSE) {
  # plot(xx$mu$comp1)
  stopifnot(is.theta(theta))
  stopifnot(is.integer(which.dims))
  stopifnot(length(which.dims)==2)
  stopifnot(all(which.dims <= theta$d))

  # Subset theta to relevant dimensions
  new_theta <- theta
  new_theta$d <- 2
  new_theta$mu <- lapply(theta$mu, "[", which.dims)
  new_theta$sigma <- lapply(theta$sigma, function(m) m[which.dims,which.dims])

  # Wrapper for GMM log-likelihood
  loglik <- function(x, y) {
    GMCM:::dgmm.loglik(new_theta, z = cbind(x, y), marginal.loglik = TRUE)
  }

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
  res2 <- outer(x2, y2, FUN = loglik)

  # Capture ...
  additional.args <- list(...)
  if (!("col" %in% names(additional.args))) {
    additional.args$col <- "#FF000080"
  }

  # Perform the plotting operations
  do.call("contour", c(list(x = x2, y = y2, z = res2), additional.args))

  if (add.means) {
    points(do.call("rbind", new_theta$mu), pch = 16, cex = .6)
  }

  if (add.ellipses) {

    for (comp in seq_len(new_theta$m)) {
      ell <- ellipse::ellipse(theta$sigma[[comp]],
                              which = which.dims,
                              centre = theta$mu[[comp]])
      lines(ell, lwd = 2, col = "red")
    }
  }

  return(invisible(list(x = x2, y = y2, z = res2)))
}
