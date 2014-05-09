\name{dmvnormal}
\alias{dmvnormal}
\alias{rmvnormal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multivariate Gaussian probability density function
}
\description{
Fast simulation and evalutation of multivariate Gaussian probability densities.
}
\usage{
rmvnormal(n, mu, sigma)
dmvnormal(x, mu, sigma)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
 A p times k matrix of quantiles. Each rows correspond to a realization from the density and each column corresponds to a dimension.
}
  \item{n}{
  The number of observations to be simulated.
  }
  \item{mu}{
 The mean vector of dimension k. 
}
  \item{sigma}{
 The variance-covariance matrix of dimension k times k.
}
}
\details{
\code{dmvnormal} functions similarly to \code{dmvnorm} from the \code{mvtnorm}-package.
}
\value{
\code{dmvnorm} returns a 1 by p matrix of the probability densities corresponding to each row of x.

\code{rmvnorm} returns a p by k matrix of observations from at multivariate normal distribution with the given mean \code{mu} and covariance \code{sigma}. Each row corresponds to an observation.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Anders Ellern Bilgrau
}
%\note{
% 
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{dmvnorm} and \code{rmvnorm} in the \code{mvtnorm}-package.
}
\examples{
GMCM:::dmvnormal(x = matrix(rnorm(300), 100, 3),
                 mu = 1:3,
                 sigma = diag(3))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
