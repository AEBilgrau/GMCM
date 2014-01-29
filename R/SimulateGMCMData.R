SimulateGMCMData <- function (n = 1000, par, d = 2, theta, ...) {
  if (missing(theta) & missing(par)) {
    theta <- rtheta(d = d, ...)
  } else if (missing(theta) & !missing(par)) {
    theta <- meta2full(par, d = d)
  }
  gmm.data <- SimulateGMMData(n = n, theta = theta, ...)
  u        <- pgmm.marginal(gmm.data$z, theta = theta)
  return(list(u = u, z = gmm.data$z, K = gmm.data$K, theta = theta))
}


