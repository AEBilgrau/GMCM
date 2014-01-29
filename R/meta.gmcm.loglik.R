meta.gmcm.loglik <- function (par, u, rescale = TRUE, positive.rho = TRUE) {
  # par is a vector of transformed (if rescale == TRUE) parameters
  if (rescale)
    par <- tt(par, ncol(u), positive.rho = positive.rho)
  return(dgmcm.loglik(theta = meta2full(par, ncol(u)), u))
}