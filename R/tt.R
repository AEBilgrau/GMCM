tt <- function(tpar, d, positive.rho) {
  par      <- NA
  par[1]   <- inv.logit(tpar[1])
  par[2:3] <- exp(tpar[2:3])
  if (positive.rho) {
    par[4] <- inv.logit(tpar[4])
  } else {
    par[4] <- inv.rho.transform(tpar[4], d)
  }
  return(par)
}