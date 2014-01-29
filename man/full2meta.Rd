\name{full2meta}
\alias{meta2full}
\alias{full2meta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert between parameter formats
}
\description{
These functions converts the parameters between the unconstrained Gaussian mixture (copula) model of Tewari et. al. (2011) and the constrained model of Li et. al. (2011). Most functions of the GMCM packages use the \code{theta} format described in \code{\link{rtheta}}.
}
\usage{
full2meta(theta)
meta2full(par, d)
}
\arguments{
  \item{theta}{
A list of parameters for the full model. Formatted as described in \code{\link{rtheta}}.
}
  \item{par}{
A vector of length 4 where \code{par[1]} is the probability of coming from the first component, \code{par[2]} is the mean value, \code{par[3]} is the standard deviation, and \code{par[4]} is the correlation of the reproducible component.
}
  \item{d}{
The dimension of the mixture distribution.
}
}
\details{
If a \code{theta} is supplied which is not on the form of Li et. al. (2011) the output is coerced by simply picking the first elements of the first mean vector and first covariance matrix as mean and standard deviation, respectively.
}
\value{
\code{full2meta} returns a numeric vector of length 4 formatted as \code{par}.

\code{meta2full} returns a formatted list of parameters as \code{\link{rtheta}}.
}
\references{
Li, Q., Brown, J. B. J. B., Huang, H., & Bickel, P. J. (2011). Measuring reproducibility of high-throughput experiments. The Annals of Applied Statistics, 5(3), 1752-1779. doi:10.1214/11-AOAS466

Tewari, A., Giering, M., & Raghunathan, A. (2011). Parametric Characterization of Multimodal Distributions with Non-gaussian Modes. IEEE 11th International Conference on Data Mining Workshops, 2011, 286-292. doi:10.1109/ICDMW.2011.135
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
theta <- GMCM:::rtheta(m = 2, d = 2)
print(par <- full2meta(theta))
print(theta.special.case <- meta2full(par, d = 2))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
