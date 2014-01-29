\name{EStep}
\alias{EStep}
\alias{MStep}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Steps of the EM algorithm for a Gaussian Mixture model
}
\description{
Functions to perform the expectation and maximization steps of the EM algorithm for an multivariate Gaussian mixture model.
}
\usage{
EStep(x, theta)
MStep(x, kappa, meta.special.case = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A matrix of observations where rows corresponds to features and columns to experiments.
}
  \item{theta}{
A list of parameters formatted as described in \code{\link{rtheta}}.
}
  \item{kappa}{
A matrix where the (i,j)'th entry is the probability that \code{x[i,]} belongs to the \code{j}'th component. Usually the returned value of \code{EStep}.
}
  \item{meta.special.case}{
Logical. If \code{TRUE}, the maximization step is performed under the special case of Li et. al. (2011). Default values is \code{FALSE}.
}
}
%\details{
% Details
%}
\value{
\code{EStep} returns a matrix of probabilities as \code{kappa} above.

\code{MStep} returns a list of parameters formatted as described in \code{\link{rtheta}}.
}
\references{
Li, Q., Brown, J. B. J. B., Huang, H., & Bickel, P. J. (2011). Measuring reproducibility of high-throughput experiments. The Annals of Applied Statistics, 5(3), 1752-1779. doi:10.1214/11-AOAS466
}
\author{
Anders Ellern Bilgrau (abilgrau@math.aau.dk)
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{rtheta}}
}
\examples{
set.seed(1)
sim <- GMCM:::SimulateGMMData(n = 100)
x <- sim$z
true.theta <- sim$theta
init.theta <- GMCM:::rtheta()  # Generate starting parameters

# Do one EM interation
es <- GMCM:::EStep(x, init.theta)
new.theta <- GMCM:::MStep(x, es)

# Compare current estimate with the true
new.theta
true.theta
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
