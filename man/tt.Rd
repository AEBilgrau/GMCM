\name{tt}
\alias{tt}
\alias{inv.tt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Reparametrization of GMCM parameters.
}
\description{
  These functions map the four GMCM parameters in the model of Li et. al. (2011) onto the real line and back. The mixture proportion is logit transformed. The mean and standard deviation are log transformed. The correlation is translated and scaled to the interval (0,1) and logit transformed by \code{\link{rho.transform}}.
}
\usage{
tt(tpar, d, positive.rho)
inv.tt(par, d, positive.rho)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tpar}{
  A vector of length 4 of the transformed parameters where \code{tpar[1]} corresponds to the mixture proportion, \code{tpar[2]} the mean, \code{tpar[3]} the standard deviation, and \code{tpar[4]} the correlation.
}
  \item{par}{
  A vector of length 4 where \code{par[1]} is the mixture proportion, \code{tpar[2]} the mean, \code{tpar[3]} the standard deviation, and \code{tpar[4]} the correlation.
}
  \item{d}{
The dimension of the space.
}
  \item{positive.rho}{
Logical. If \code{TRUE}, the correlation is transformed by a simple \code{\link{logit}} transformation. If \code{FALSE} the \code{\link{rho.transform}} is used.
}
}
\details{
The functions are used only in the wrapper to \code{optim} when the GMCM log-likelihood is optimized.

\code{par[1]} should be between 0 and 1.
\code{par[2]} and \code{par[3]} should be non-negative.
If \code{positive.rho} is \code{FALSE}, \code{par[4]} should be between \eqn{-1/(d-1)}{-1/(d-1)} and 1. Otherwise, \code{positive.rho} should be between 0 and 1.
}
\value{
A vector of the transformed or inversely transformed values of length 4.

\code{tt} returns \code{par} as described above.

\code{inv.tt} returns \code{tpar} as described above.
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

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
par <- c(pie1 = 0.3, mu = 2, sigma = 0.5, rho = 0.8)
tpar <- GMCM:::inv.tt(par, d = 3, positive.rho = FALSE)
GMCM:::tt(tpar, d = 3, positive.rho = FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
