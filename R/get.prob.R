get.prob <- function (x, theta, ...) {
  pseudo.data <- qgmm.marginal(u = x, theta = theta, ...)
  kappa <- EStep(x = pseudo.data, theta = theta)
  return(kappa)
}