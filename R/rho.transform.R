rho.transform <- function (rho, d) { # transformation of rho
  #if (any(rho > 1 | rho < -1/(d-1)))
  #  stop("rho is not in the interval -1/(d-1) to 1")
  #t <- 2*rho*(d-1)/d - (d-2)/d
  #return(tan(t*pi/2))
  return(logit((rho*(d - 1) + 1)/d))
}