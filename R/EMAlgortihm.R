EMAlgorithm <- function (x, theta, eps = 10^-6, max.ite = 10^5,
                         trace.theta = FALSE, verbose = FALSE) {
  loglik.tr <- c(dgmm.loglik(theta, x))
  theta.tr  <- vector("list", 1); theta.tr[[1]] <- theta
  for (k in 2:max.ite) {
    kappa <- EStep(x = x, theta = theta)
    theta <- MStep(x = x, kappa = kappa)
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
      warning(paste("Max (", max.ite, ") iterations reached", sep = ""))
  }
  res <- list(theta = theta, 
              loglik.tr = loglik.tr, 
              kappa = kappa, 
              theta.tr = theta.tr)
  if (!trace.theta) 
    res <- res[-4]
  return(res)
}
