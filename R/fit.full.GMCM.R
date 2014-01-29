fit.full.GMCM <- function (u, 
                           m,
                           theta = choose.theta(u, m),
                           method = c("NM", "SANN", "L-BFGS", "L-BFGS-B", "PEM"),
                           max.ite = 1000, 
                           verbose = TRUE,
                           ...) {
  # Note, Uhat is idempotent. Hence, already ranked data will not change
  u <- Uhat(u)

  method <- gsub("NM", "Nelder-Mead", match.arg(method)) 
  
  if (missing(m) & missing(theta)) {
    stop("m is not supplied.")
  }

  if (method != "PEM") {
    
    gmcm.loglik <- function (par, u, m) { # Defining objective function
      #cat("par=", par, "\n")  # FOR DEBUGGING
      theta <- par2theta(par, d = ncol(u), m = m)
      theta$pie <- theta$pie/sum(theta$pie)
      #cat("theta=", unlist(theta$sigma), "\n")  # FOR DEBUGGING
      loglik <- dgmcm.loglik(theta = theta, u)

      return(loglik)
    }

    par <- theta2par(theta)
    fit <- optim(par, gmcm.loglik, u = u, m = theta$m,
                 control = list(maxit = max.ite, 
                                fnscale = -1, trace = verbose, ...), 
                 method = method)
    theta <- par2theta(fit$par, d = theta$d, m = theta$m)
    theta$pie <- theta$pie/sum(theta$pie)

    return(theta)
    
  } else {
    
    fit <- PseudoEMAlgorithm(x = u, 
                             theta = theta, 
                             max.ite = max.ite, 
                             verbose = verbose,
                             meta.special.case = FALSE,
                             ...)
    
    return(fit$theta)
  }
}