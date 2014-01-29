fit.meta.GMCM <- function(u,
                          init.par,
                          method = c("NM", "SANN", "L-BFGS", "L-BFGS-B", "PEM"),
                          verbose = TRUE,
                          max.ite = 1000,
                          positive.rho = TRUE,
                          trace.theta = FALSE,
                          ...) 
{
  d <- ncol(u)
  
  # Note, Uhat is idempotent. Hence, already ranked data will not change
  u <- Uhat(u)
  
  switch(match.arg(method), 
         "NM" = {  # Fitted using Nelder-Mead (The amoeba method)
           fit <- optim(inv.tt(init.par, d = d, positive.rho = positive.rho),
                        meta.gmcm.loglik, u = u, 
                        positive.rho = positive.rho, 
                        rescale = TRUE, method = "Nelder-Mead",
                        control = list(maxit = max.ite,
                                       trace = verbose, 
                                       fnscale = -1, ...))
           fitted.par <- tt(fit$par, d = d, positive.rho = positive.rho)
         }, 
         "SANN" = {  # Fitting using Simulated Annealing
           fit <- optim(inv.tt(init.par, d = d, positive.rho = positive.rho),
                        meta.gmcm.loglik,  u = u, positive.rho = positive.rho,
                        rescale = TRUE, method = "SANN",
                        control = list(maxit = max.ite, 
                                       trace = verbose, 
                                       fnscale = -1, ...))
           fitted.par <- tt(fit$par, d = d, positive.rho = positive.rho)
         },
         "L-BFGS" = { # Fit via L-BFGS-B (Limited-memory quasi-Newton method)
           fit <- optim(inv.tt(init.par, d = d, 
                               positive.rho = positive.rho),
                        meta.gmcm.loglik, u = u, positive.rho = positive.rho, 
                        rescale = TRUE, method = "L-BFGS-B",
                        control = list(maxit = max.ite, 
                                       trace = verbose, 
                                       fnscale = -1, ...))
           fitted.par <- tt(fit$par, d = d, positive.rho = positive.rho)
         },
         "L-BFGS-B" = {  # Fitting using L-BFGS-B !! NOT RESCALED !!
           fit <- optim(init.par,
                        meta.gmcm.loglik, u = u, 
                        positive.rho = positive.rho,
                        rescale = FALSE, method = "L-BFGS-B",
                        upper = c(1,Inf,Inf,1), 
                        lower = c(0,0,0, ifelse(positive.rho, 0, -1/(d-1))),
                        control = list(maxit = max.ite,
                                       trace = verbose, 
                                       fnscale = -1, ...))
           fitted.par <- fit$par
         },
         "PEM" = {  # Fitting using the Li Pseudo EM Algorithm
           fit <- PseudoEMAlgorithm(u, theta = meta2full(init.par, d = d),
                                    max.ite = max.ite,
                                    meta.special.case = TRUE,
                                    trace.theta = trace.theta,
                                    verbose = verbose,
                                    ...)
           fitted.par <- full2meta(fit$theta)
         })
  names(fitted.par) <- c("pie1", "mu", "sigma", "rho")
  if (trace.theta)
    fitted.par <- list(fitted.par, fit)
  return(fitted.par)
}