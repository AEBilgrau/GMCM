\name{dgmcm.loglik}
\alias{dgmcm.loglik}
\alias{dgmm.loglik}
\alias{dgmm.loglik.marginal}
\alias{pgmm.marginal}
\alias{qgmm.marginal}

\title{
Probability, density, and likelihood functions of the Gaussian mixture (copula) model
}
\description{
   Marginal and simulataneuos cumulative distribution, log probability density, and log-likelihood functions of the Gaussian mixture model (GMM) and Gaussian mixture copula model (GMCM) and the relevant inverse marginal quantile functions.
}
\usage{
dgmcm.loglik(theta, u, marginal.loglik = FALSE, ...)
dgmm.loglik(theta, z, marginal.loglik = FALSE) 
dgmm.loglik.marginal(theta, x, marginal.loglik = TRUE)
pgmm.marginal(z, theta)
qgmm.marginal(u, theta, res = 1000, spread = 5) 
}
\arguments{
  \item{theta}{
  A list parameters as described in \code{\link{rtheta}}.
}
  \item{z}{
  A matrix of realizations from the latent process where each row corresponds to an observation.
}
  \item{u}{
  A matrix of (estimates of) realizations from the GMCM where each row corresponds to an observation.
}
  \item{marginal.loglik}{
  Logical. If \code{TRUE}, the marginal log-likelihood functions for each multivariate observation (i.e. the log densities) are returned. In other words, if \code{TRUE} the sum of the marginal likelihoods is not computed.
}
 \item{x}{
  A matrix where each row corresponds to an observation.
}
 \item{res}{
 The resolution at which the inversion of \code{qgmm.marginal} is done. Default is 1000.
}
 \item{spread}{
 The number of marginal standard deviations from the marginal means the \code{pgmm.marginal} is to be evaluated on.
}
 \item{\dots}{
 Arguments passed to \code{qgmm.marginal}.
 }
}
\details{
\code{qgmm.marginal} distributes approximately \code{res} points around the cluster centers according to the mixture proportions in \code{theta$pie} and evaluates \code{pgmm.marginal} on these points. An approximate inverse of \code{pgmm.marginal} function is contructed by linear interpolation of the flipped evaluated coordinates.
}
\value{
The returned value depends on the value of \code{marginal.loglik}.
If \code{TRUE}, the unsummed marginal likelihood values are returned.
If \code{FALSE}, the scalar sum log-likelihood is returned.

\code{dgmcm.loglik}: As above, with the GMCM density.

\code{dgmm.loglik}: As above, with the GMM density.

\code{dgmm.loglik.marginal}: As above, where the j'th element is evaluated in the j'th marginal GMM density.

\code{pgmm.marginal}: A matrix where the (i,j)'th entry is the (i,j)'th entry of \code{z} evaluated in the jth marginal GMM density.

\code{qgmm.marginal}: A matrix where the (i,j)'th entry is the (i,j)'th entry of \code{u} evaluated in the inverse jth marginal GMM density.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Anders Ellern Bilgrau (abilgrau@math.aau.dk)
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%\code{\link{help}}, ~~~
%}
\examples{

set.seed(1)
data <- SimulateGMCMData(n = 10)
u <- data$u
z <- data$z
print(theta <- data$theta)

GMCM:::dgmcm.loglik(theta, u, marginal.loglik = FALSE)
GMCM:::dgmcm.loglik(theta, u, marginal.loglik = TRUE)

GMCM:::dgmm.loglik(theta, z, marginal.loglik = FALSE)
GMCM:::dgmm.loglik(theta, z, marginal.loglik = TRUE)

GMCM:::dgmm.loglik.marginal(theta, z, marginal.loglik = FALSE)
GMCM:::dgmm.loglik.marginal(theta, z, marginal.loglik = TRUE)

GMCM:::pgmm.marginal(z, theta)
GMCM:::qgmm.marginal(u, theta)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
