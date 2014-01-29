PseudoEMAlgorithm <- function (x, theta, 
                               eps = 1e-4,
                               max.ite = 1e3,
                               verbose = FALSE,
                               trace.theta = FALSE,
                               meta.special.case = FALSE,
                               convergence.criterion = 
                                 c("absGMCM", "GMCM", "GMM", "Li", "absLi")) {
  cc <- match.arg(convergence.criterion)
  
  u <- Uhat(x)
  z <- qgmm.marginal(u, theta)
  loglik.tr           <- matrix(NA, 3, max.ite)
  loglik.tr[2, 1]     <- dgmm.loglik(theta, z)
  loglik.tr[3, 1]     <- dgmcm.loglik(theta, u)
  rownames(loglik.tr) <- c("gmm.pre", "gmm.post", "gmcm")
  theta.tr            <- list(theta)
  
  for (k in 2:max.ite) {
    z <- qgmm.marginal(u, theta)
    loglik.pre <- dgmm.loglik(theta, z)    # Compute loglik pre EM step
    kappa <- EStep(x = z, theta = theta)
    if (any(colSums(kappa) == 0)) {
      stop("No observations are estimated to be from component(s): ",
           paste(which(colSums(kappa) == 0), collapse = " and "), ". ",
           "All posterior probabilities are zero. ",
           "Try another start estimate or fewer components.")
    }
    theta <- MStep(x = z, kappa = kappa,
                   meta.special.case = meta.special.case)
    loglik.post      <- dgmm.loglik(theta, z)   # Compute loglik post EM step
    loglik.tr[1:2,k] <- c(loglik.pre, loglik.post)  # Tracking of pre/post theta
    loglik.tr[3,  k] <- dgmcm.loglik(theta, u)
    theta.tr[[k]] <- theta
    
    delta <- 
      switch(cc, 
             "GMCM"    = loglik.tr[3, k] - loglik.tr[3, k-1],
             "absGMCM" = abs(loglik.tr[3, k] - loglik.tr[3, k-1]),
             "GMM"     = loglik.post - loglik.pre,
             "Li"      = loglik.tr[2, k]- loglik.tr[3, k-1],
             "absLi"   = abs(loglik.tr[2, k]- loglik.tr[3, k-1]))
    
    if (verbose) {
      cat(k, 
          "| delta =", sprintf("%.6f", delta),
          "| gmm =", sprintf("%.2f", round(loglik.post,2)), 
          "| gmcm = ", sprintf("%.2f", round(loglik.tr[3,k],2)),"\n")
      flush.console()
    }
    
    if (delta < eps) {
      break
    }
  }
  
  if (k == max.ite)  # Warn if maximun interations reached.
    warning(paste("Max iterations (", max.ite, ") reached.", sep = ""))
  
  if (cc %in% c("absGMCM", "absLi")) {
    k <- which.max(loglik.tr[3, ])
    theta <- theta.tr[[k]]
  }
  loglik.tr <- loglik.tr[, !apply(is.na(loglik.tr), 2, all)]
  res <- list(theta = theta, loglik.tr = loglik.tr,
              kappa = kappa, theta.tr  = theta.tr)
  if (!trace.theta)
    res <- res[-4]
  return(res)
}