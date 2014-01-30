dgmcm.loglik <- function (theta, u, marginal.loglik = FALSE, ...) {
  z      <- qgmm.marginal(rbind(u), theta, ...)
  tmp    <- dgmm_loglik_marginal(mus = theta$mu, sigmas = theta$sigma, 
                                 pie = theta$pie, z = z, 
                                 marginal_loglik = marginal.loglik)
  loglik <- dgmm_loglik(mus = theta$mu, sigmas = theta$sigma, pie = theta$pie, 
                        z = z, marginal_loglik = marginal.loglik) - rowSums(tmp)
  if (!marginal.loglik)
    loglik <- sum(loglik)
  return(loglik)
}

# dgmcm.loglik2 <- function (theta, u, marginal.loglik = FALSE, ...) {
#  z      <- qgmm.marginal(rbind(u), theta, ...)
#  tmp    <- dgmm.loglik.marginal(theta, z)  
#  loglik <- dgmm.loglik(theta, z, marginal.loglik = TRUE) - rowSums(tmp)
#  if (!marginal.loglik)
#    loglik <- sum(loglik)
#  return(loglik)
# }

# dgmcm.loglik3 <- function (theta, u, marginal.loglik = FALSE, ...) {
#   am <- function(u) {
#     if (is.null(dim(u)))
#       dim(u) <- c(1, length(u))
#     return(u)
#   }
#   z      <- am(qgmm.marginal(am(u), theta, ...))
#   tmp    <- am(dgmm.loglik.marginal(theta, z))
#   loglik <- dgmm.loglik(theta, z, marginal.loglik = TRUE) - rowSums(tmp)
#   if (!marginal.loglik)
#     loglik <- sum(loglik)
#   return(loglik)
# }