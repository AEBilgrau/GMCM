#' Logit and inverse logit transforms
#' 
#' The logit transformation (i.e. the log of the odds) and its inverse.
#' 
#' 
#' @aliases logit inv.logit
#' @param p A vector of probabilities.
#' @param a A vector of real values.
#' @return \code{logit} returns a vector of the same length as \code{p} with
#' the log odds of \code{p}.
#' 
#' \code{inv.logit} returns a vector of the same length as \code{a} of the
#' inverse logit transformed values. This function is also known as the
#' expit-function.
#' @author Anders Ellern Bilgrau (abilgrau@@math.aau.dk)
#' @seealso Used in \code{\link{tt}} and \code{\link{inv.tt}}.
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' p <- runif(100)
#' print(a <- GMCM:::logit(p))
#' p - GMCM:::inv.logit(a)
#' 
logit <- function(p) # logit function
  log(p/(1-p))
