EStep <- function (x, theta) {
  return(EStepRcpp(x, mus = theta$mu, sigmas = theta$sigma, pie = theta$pie))
}
# Old EStep:
# EStep <- function (x, theta) {
#   g <- function(j) {
#     theta$pie[j]*dmvnormal(x, mu = theta$mu[[j]], sigma = theta$sigma[[j]])
#   }
#   f <- sapply(1:theta$m, g)
#   kappa <- f/rowSums(f)
#   kappa[is.nan(kappa[,1]) | is.nan(kappa[,2]), ] <- 0  # What if kappa has more than 3 cols, fix!
#   return(kappa)
# }