inv.rho.transform <- function (a, d) { 
  # inverse transformation of rho
  #tmp <- 2*atan(a)/pi
  #return((tmp+(d-2)/d)/(2*(d-1)/d))
  return((d*inv.logit(a)-1)/(d-1))
}