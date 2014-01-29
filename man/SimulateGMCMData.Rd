\name{SimulateGMCMData}
\alias{SimulateGMCMData}
\alias{SimulateGMMData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulation from the Gaussian mixture (copula) model
}
\description{
Functions to simulate data from the Gaussian mixture model (GMM) and the Gaussian mixture copula model (GMCM).
}
\usage{
SimulateGMCMData(n = 1000, par, d = 2, theta, ...)
SimulateGMMData(n = 1000, theta = rtheta(...), ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
  Integer. The number of realizations drawn from the model. Default is 1000.
}
  \item{par}{A vector of parameters of length 4 where \code{par[1]} is the mixture proportion, \code{par[2]} is the mean, \code{par[3]} is the standard deviation, and \code{par[4]} is the correlation.
}
  \item{d}{
  The number of dimensions (or, equivalently, experiments) in the mixture distribution.
}
  \item{theta}{
  A list of parameters for the model as described in \code{\link{rtheta}}.
}
  \item{\dots}{
  In \code{SimulateGMCMData} the arguments are passed to \code{SimulateGMMData}.
  In \code{SimulateGMMData} the arguments are passed to \code{\link{rtheta}}.
}
}
\details{
The \code{par} argument specifies the parameters of the Li et. al. (2011) GMCM.
The \code{theta} argument specifies an arbitrary GMCM. Either one can be supplied.  If both are missing, random parameters are chosen for the general model.
}
\value{
\code{SimulateGMCMData} returns a list of length 4 with elements:
\item{u }{A matrix of the realized values of the GMCM.}
\item{z }{A matrix of the latent GMM realizations.}
\item{K }{An integer vector denoting the component from which the realization comes.}
\item{theta }{A list containing the used parameters for the simulations with the format described in \code{\link{rtheta}}.}
\code{SimulateGMMData} returns a list of length 3 with elements:
\item{z}{A matrix of GMM realizations.}
\item{K}{An integer vector denoting the component from which the realization comes.}
\item{theta}{As above and in \code{\link{rtheta}}.}
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

\seealso{
\code{\link{rtheta}}
}
\examples{
set.seed(2)

# Simulation from the GMM
gmm.data1 <- SimulateGMMData(n = 200, m = 3, d = 2)
str(gmm.data1)

# Plotting the simulated data
plot(gmm.data1$z, col = gmm.data1$K)

# Simulation from the GMCM
gmcm.data1 <- SimulateGMCMData(n = 1000, m = 4, d = 2)
str(gmcm.data1)

# Plotthe 2nd simulation
par(mfrow = c(1,2))
plot(gmcm.data1$z, col = gmcm.data1$K)
plot(gmcm.data1$u, col = gmcm.data1$K)

# Simulation from the special case of GMCM
theta <- meta2full(c(0.7, 2, 1, 0.7), d = 3)
gmcm.data2 <- SimulateGMCMData(n = 5000, theta = theta)
str(gmcm.data2)

# Plotting the 3rd simulation
par(mfrow=c(1,2))
plot(gmcm.data2$z, col = gmcm.data2$K)
plot(gmcm.data2$u, col = gmcm.data2$K)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
