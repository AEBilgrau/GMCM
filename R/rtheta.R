rtheta <- function (m = 3, d = 2) {
#   getPositiveDefMat <- function(d) {
#     mat <- matrix(0, d, d)
#     mat[lower.tri(mat, diag = TRUE)] <- 
#       rnorm(sum(lower.tri(mat, diag = TRUE)), mean = 0, sd = 4)
#     diag(mat) <- abs(diag(mat))
#     return(mat %*% t(mat))
#   }
  getPosDefMat <- function(d) {
    spherePointPicking <- function(n, d) {
      # n points on the d-dimensional hypersphere
      x <- replicate(d, rnorm(n))
      return(x/sqrt(rowSums(x^2)))
    }
    r <- abs(rnorm(d, mean = 0, sd = 8))
    basis <- t(spherePointPicking(n = d, d = d))
    decomp <- qr(basis)
    Q <- qr.Q(decomp)
    D <- diag(r)
    return(t(Q)%*%D%*%Q)
  }
  
  pie <- runif(m)
  pie <- pie/sum(pie)
  names(pie) <- paste("pie", 1:m, sep = "")
  mu <- replicate(m, rnorm(d, sd = 10), simplify = FALSE)
  sigma <- replicate(m, getPosDefMat(d), simplify = FALSE)
  #sigma <- replicate(m, genPositiveDefMat(d)$Sigma, simplify = FALSE)
  names(sigma) <- names(mu) <- paste("comp", 1:m, sep = "")
  rtheta <- list(m = m, d = d, pie = pie, mu = mu, sigma = sigma)
  return(rtheta)
}




