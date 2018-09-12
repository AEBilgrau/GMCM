#' Heuristically chosen starting value of theta
#'
#' This function uses a \code{k}-means algorithm to heuristically select
#' suitable starting values for the general model.
#'
#' The function selects the centers from the k-means algorithm as an initial
#' estimate of the means. The proportional sizes of the clusters are selected
#' as the initial values of the mixture proportions. The within cluster
#' standard deviations are squared and used as the variance of the clusters
#' within each dimension. The
#' correlations between each dimension are taken to be zero.
#'
#' @param u A matrix of (estimates of) realizations from the GMCM.
#' @param m The number of components to be fitted.
#' @param no.scaling Logical. If TRUE, no scaling of the means and
#'   variance-covariance matrices is done.
#' @param \dots Arguments passed to \code{\link{kmeans}}.
#' @return A list of parameters for the GMCM model on the form described in
#'   \code{\link{rtheta}}.
#' @note The function uses the \code{kmeans} function from the
#'   \code{stats}-package.
#' @author Anders Ellern Bilgrau <anders.ellern.bilgrau@@gmail.com>
#' @examples
#' set.seed(2)
#'
#' # Simulating data
#' data1 <- SimulateGMCMData(n = 10000, m = 3, d = 2)
#' obs.data <- Uhat(data1$u)  # The ranked observed data
#'
#' # Using choose.theta to get starting estimates
#' theta <- choose.theta(u = obs.data, m = 3)
#' print(theta)
#'
#' # To illustrate theta, we can simulate from the model
#' data2 <- SimulateGMMData(n = 10000, theta = theta)
#'
#' cols <- apply(get.prob(obs.data,theta),1,which.max)
#'
#' # Plotting
#' par(mfrow = c(1,3))
#' plot(data1$z, main = "True latent GMM")
#' plot(Uhat(data1$u), col = cols,
#'      main = "Observed GMCM\nColoured by k-means clustering")
#' plot(data2$z, main = "initial GMM")
#'
#' # Alteratively, theta can simply be plotted to illustrate the GMM density
#' dev.off()
#' plot(theta, add.ellipses = TRUE)
#' points(data2$z, pch = 16, cex = 0.4)
#' @export
choose.theta <- function(u,
                         m,
                         no.scaling = FALSE,
                         ...) {

  # Get K-means estimate
  km  <- kmeans(u, centers = m, ...)

  # Estimate pie
  pie <- km$size/sum(km$size)

  # Extract mu and translate clusters to such that cluster 1 have zero mean
  mu <- lapply(1:m, function(i) km$centers[i, ])
  mu <- lapply(mu, "-", mu[[1]])           # Translating means

  # Get sds
  get.sds <- function(i) {
    colSds(u[km$cluster == i, , drop = FALSE])
  }
  sds <- lapply(1:m, get.sds)

  # Construct sigma
  sigma <- lapply(sds, function(sd) diag(sd^2))

  # Scaling to unit variances in component 1 and scaling means
  if (!no.scaling) {
    scaling <- sds[[1]]                           # Get scaling factor (sds)
    mscaling <- tcrossprod(scaling)               # Get "matrix" scaling factor
    sigma   <- lapply(sigma, "/", mscaling)       # Scaling sigma
    mu      <- lapply(mu, "/", scaling)           # Scaling means
  }

  # Naming
  names(pie) <- paste("pie", 1:m, sep = "")
  names(mu) <- names(sigma) <- paste("comp", 1:m, sep = "")
  ans <- structure(list(m = m, d = ncol(u), pie = pie, mu = mu, sigma = sigma),
                   class = "theta")

  return(ans)
}
