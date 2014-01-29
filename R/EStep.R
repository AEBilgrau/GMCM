EStep <- function (x, theta) {
  g <- function(j) {
    theta$pie[j]*dmvnormal(x, mu = theta$mu[[j]], sigma = theta$sigma[[j]])
  }
  f <- sapply(1:theta$m, g)
  kappa <- f/rowSums(f)
  kappa[is.nan(kappa[,1]) | is.nan(kappa[,2]), ] <- 0
  return(kappa)
}