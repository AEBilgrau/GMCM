#' EM algorithm for Gaussian mixture models
#'
#' The regular expectation-maximization algorithm for general multivariate
#' Gaussian mixture models.
#'
#' @details Though not as versatile, the algorithm can be a faster alternative
#' to \code{Mclust} in the \code{mclust}-package. If \code{theta} is not given,
#' a k-means clustering is used to determine the initial \code{theta}.
#'
#' @param x A \code{matrix} of observations where each row correspond to an
#'   observation and each columns to a feature/variable.
#' @param theta A list of parameters of class \code{theta} as described in
#'   \code{\link{rtheta}}.
#'   Optional. If not provided \code{m} should be given.
#' @param m \code{numeric}. The number of components if \code{theta} is not
#'   supplied.
#' @param eps The maximal required difference in successive likelihoods to
#'   establish convergence.
#' @param max.ite The maximum number of iterations.
#' @param trace.theta Logical. If \code{TRUE}, all estimates are stored and
#'   returned. Default is \code{FALSE}.
#' @param verbose Set to \code{TRUE} for verbose output. Default is
#' \code{FALSE}.
#' @return
#'   A list of length 3 with elements:
#'   \item{theta }{A list of the estimated parameters as described in
#'                 \code{\link{rtheta}}.}
#'   \item{loglik.tr}{A numeric vector of the log-likelihood trace.}
#'   \item{kappa }{A matrix where \code{kappa[i,j]} is the probability that
#'                 \code{x[i, ]} is realized from the \code{j}'th component.}
#' @author Anders Ellern Bilgrau <anders.ellern.bilgrau@@gmail.com>
#' @seealso \code{\link{rtheta}}, \code{\link{PseudoEMAlgorithm}}
#' @examples
#'
#' set.seed(10)
#' data <- SimulateGMCMData(n = 1000, d = 2, m = 3)
#' start.theta <- rtheta(d = 2, m = 3)
#' res <- GMCM:::EMAlgorithm(data$z, theta = start.theta)
#'
#' par(mfrow = c(1,2))
#' plot(data$z, cex = 0.5, pch = 16, main = "Simulated data",
#'      col = rainbow(3)[data$K])
#' plot(data$z, cex = 0.5, pch = 16, main = "GMM clustering",
#'      col = rainbow(3)[apply(res$kappa,1,which.max)])
#'
EMAlgorithm <- function (x, theta, m, eps = 1e-6, max.ite = 1e5,
                         trace.theta = FALSE, verbose = FALSE) {
  if (missing(m) && missing(theta)) {
    stop("Either m or theta should be provided.")
  }
  if (!missing(m) && !missing(theta)) {
    stopifnot(m == theta$m)
  }
  if (missing(theta) && !missing(m)) {
    theta <- rtheta(m = m, d = ncol(x), method = "EqualSpherical")
    km <- kmeans(x, centers = m)
    theta$mu[] <- lapply(seq_len(m), function(j) km$centers[j, ])
    theta$pie[] <- km$size/sum(km$size)
    theta$sigma[] <- lapply(seq_len(m), function(j) cov(x[km$cluster == m, ]))
  }

  loglik.tr <- c(dgmm.loglik(theta, x))
  theta.tr  <- vector("list", 1)
  theta.tr[[1]] <- theta
  for (k in 2:max.ite) {
    kappa <- EStep(x = x, theta = theta)
    theta <- MStep(x = x, kappa = kappa)
    if (k == 2 && any(low.prob <- colSums(kappa) < 1e-15)) {
      stop("No observations are estimated to be from component(s): ",
           paste(which(low.prob), collapse = ", "), ". ",
           "All posterior probabilities are zero. ",
           "Try another start estimate or fewer components.")
    }
    loglik.tr[k] <- dgmm.loglik(theta, x)
    theta.tr[[k]] <- theta
    delta <- loglik.tr[k] - loglik.tr[k-1]
    if (verbose) {
      cat("iteration", k, "\tDelta loglik =", delta, "\n"); flush.console()
    }
    if (delta < 0)
      stop("Delta likelihood was negative. Something went wrong!")
    if (delta < eps)
      break
    if (k == max.ite)
      warning(paste0("Max (", max.ite, ") iterations reached"))
  }
  res <- list(theta = theta,
              loglik.tr = loglik.tr,
              kappa = kappa,
              theta.tr = theta.tr)
  if (!trace.theta)
    res <- res[-4]
  return(res)
}
