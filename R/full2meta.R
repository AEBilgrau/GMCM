# Full list to Li et al's special case
full2meta <- function(theta) {
  if (theta$m != 2) {
    stop("Too many components: m != 2")
  }
  par <- c("pie1"   = theta$pie[1],
           "mu"    = theta$mu[[2]][1],
           "sigma" = sqrt(theta$sigma[[2]][1,1]),
           "rho"   = cov2cor(theta$sigma[[2]])[1,2])
  if (par[4] <= -1/(theta$d  - 1))  
    stop("correlation coefficient is not valid")	
  return(par)
}