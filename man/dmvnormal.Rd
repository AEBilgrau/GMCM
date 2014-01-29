\name{dmvnormal}
\alias{dmvnormal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multivariate Gaussian probability density function
}
\description{
Fast evalutation of multivariate Gaussian probability density functions.
}
\usage{
dmvnormal(x, mu, sigma)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
 A n times k matrix of quantiles. Each rows correspond to a realization from the density.
}
  \item{mu}{
 The mean vector of length k. 
}
  \item{sigma}{
 The variance-covariance matrix of dimension k times k.
}
}
\details{
\code{dmvnormal} functions similarly to \code{dmvnorm} from the \code{mvtnorm}-package.
}
\value{
A 1 by n matrix of the probability density values corresponding to each row of x.
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

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
GMCM:::dmvnormal(x = matrix(rnorm(300), 100, 3),
                 mu = 1:3,
                 sigma = diag(3))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
