\name{rtheta}
\alias{rtheta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Get random parameters for the Gaussian mixture (copula) model
}
\description{
  The function generates a random set parameters for the Gaussian mixture model (GMM) and Gaussian mixture copula model (GMCM).
}
\usage{
  rtheta(m = 3, d = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{
The number of components in the mixture.
}
  \item{d}{
The dimension of the mixture distribution.
}
}
%\details{
%}
\value{
A named list of parameters with the 4 elements:
\item{m}{The number of components in the mixture. Default is 3.}
\item{d}{The dimension of the mixture distribution. Default is 2.}
\item{pie}{A numeric vector of mixture proportions which sums to one.}
\item{mu}{A list of mean vectors for each component.}
\item{comp}{A list of variance-covariance matrices for each component.}
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Anders Ellern Bilgrau (abilgrau@math.aau.dk)
}
\note{
The function \code{\link{is.theta}} checks whether or not \code{theta} is in the correct format.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{is.theta}}
}
\examples{
GMCM:::rtheta()
GMCM:::rtheta(d = 5, m = 2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
