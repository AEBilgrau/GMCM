#' Goodness of fit for the general GMCM
#'
#' Compute goodness of fit as described in \code{\link{AIC}}. The number of
#' parameters used correspond to the number of variables free to vary in the
#' general model.
#'
#' @inheritParams fit.full.GMCM
#' @param theta A list of parameters as defined in \code{\link{rtheta}}. For t
#'   this function, it will usually be the output of
#'   \code{\link{fit.full.GMCM}}.
#' @param method Which goodness of fit to compute. Default is "AIC".
#' @param k A integer specifiing the default used constant "k" in AIC. See
#'   \code{\link{AIC}}.
#'
#' @return A single number giving the goodness of fit as requested.
#' @examples
#' set.seed(1)
#' data(u133VsExon)
#' u <- Uhat(u133VsExon[sample(19577, 2000), ])  # Subset for faster fitting
#' theta1 <- fit.full.GMCM(u, m = 2, method = "L-BFGS")
#' theta2 <- fit.full.GMCM(u, m = 3, method = "L-BFGS")
#'
#' # AIC
#' goodness.of.fit(theta1, u)
#' goodness.of.fit(theta2, u)
#'
#' # BIC
#' goodness.of.fit(theta1, u, method = "BIC")
#' goodness.of.fit(theta2, u, method = "BIC")
#' @export
goodness.of.fit <- function(theta, u, method = c("AIC", "BIC"), k = 2) {
  method <- match.arg(method)

  log_likelihood <- dgmcm.loglik(theta, u, marginal.loglik = FALSE)
  npar <- length(theta2vector(theta))

  if (method == "AIC") {
    kk <- k
  } else if (method == "BIC") {
    kk <- log(nrow(u))
  }

  return(-2*log_likelihood + kk*npar)
}
