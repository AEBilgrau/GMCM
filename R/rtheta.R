#' Get random parameters for the Gaussian mixture (copula) model
#'
#' The function generates a random set parameters for the Gaussian mixture
#' model (GMM) and Gaussian mixture copula model (GMCM).
#'
#' @param m The number of components in the mixture.
#' @param d The dimension of the mixture distribution.
#' @return A named list of parameters with the 4 elements:
#'   \item{\code{m}}{An integer giving the number of components in the mixture.
#'     Default is 3.}
#'   \item{\code{d}}{An integer giving the dimension of the mixture
#'     distribution. Default is 2.}
#'   \item{\code{pie}}{A numeric vector of length \code{m} of mixture
#'     proportions between 0 and 1 which sums to one.}
#'   \item{\code{mu}}{A list of length \code{m} of numeric vectors of length
#'     \code{d} for each component.}
#'   \item{\code{sigma}}{A list of length \code{m} of variance-covariance
#'      matrices (of size \code{d} times \code{d}) for each component.}
#' @note The function \code{\link{is.theta}} checks whether or not \code{theta}
#'   is in the correct format.
#' @author Anders Ellern Bilgrau (abilgrau@@math.aau.dk)
#' @seealso \code{\link{is.theta}}
#' @examples
#' rtheta()
#' rtheta(d = 5, m = 2)
#'
#' test <- rtheta()
#' is.theta(test)
#' @export
rtheta <- function (m = 3, d = 2) {

  getPosDefMat <- function(d) {  # Function to get postive definite matrix
    spherePointPicking <- function(n, d) {
      # n uniform points on the d-dimensional hypersphere
      x <- replicate(d, rnorm(n))
      return(x/sqrt(rowSums(x^2)))
    }
    r <- abs(rnorm(d, mean = 0, sd = 8))
    basis <- t(spherePointPicking(n = d, d = d))
    decomp <- qr(basis, tol = 1e-17)  # QR decomposition
    Q <- qr.Q(decomp)
    #D <- diag(r)
    return(crossprod(Q * sqrt(r)))
    #return( t(Q) %*% D %*% Q )
  }

  #   getPositiveDefMat <- function(d) {
  #     mat <- matrix(0, d, d)
  #     mat[lower.tri(mat, diag = TRUE)] <-
  #       rnorm(sum(lower.tri(mat, diag = TRUE)), mean = 0, sd = 4)
  #     diag(mat) <- abs(diag(mat))
  #     return(mat %*% t(mat))
  #   }

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




