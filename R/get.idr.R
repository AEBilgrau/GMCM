get.idr <- function (x, theta, ...) {
  #pseudo.data <- qgmm.marginal(u = Uhat(x), theta = theta, ...)
  #kappa <- EStep(x = pseudo.data, theta = theta)
  return(get.prob(x, theta, ...)[,1])
}