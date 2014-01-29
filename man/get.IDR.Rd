\name{get.IDR}
\alias{get.IDR}
\alias{get.idr}
\alias{get.prob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Get local and adjusted irreproducibility discovery rates.
}
\description{
Functions for computing posterior cluster probabilities and local and adjusted irreproducibility discovery rates as defined by Li et. al. (2011).
}
\usage{
get.IDR(x, par, threshold = 0.05, ...) 
get.idr(x, theta, ...)
get.prob(x, theta, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  A matrix of observations where rows corresponds to features and colums to studies.
}
  \item{par}{
  A vector of length 4 where \code{par[1]} is mixture proportion of the irreproducible component, \code{par[2]} is the mean value, \code{par[3]} is the standard deviation, and \code{par[4]} is the correlation of the reproducible component.
}
  \item{threshold}{
  The thresholding level of the IDR rate.
}
  \item{theta}{
  A list of parameters for the full model as described in \code{\link{rtheta}}.
}
  \item{...}{
  Arguments passed to \code{\link{qgmm.marginal}}.
}
}
%\details{

%}
\value{
\code{get.prob} returns a matrix where entry \code{(i,j)} is the posterior probability that the observation \code{x[i, ]} belongs to cluster \code{j}.

\code{get.IDR} returns a list of length 5 with elements:
\item{idr}{A vector of the local IDR values. I.e. the posterior probability that \code{x[i, ]} belongs to the irreprodicible component.}
\item{IDR}{A vector of the adjusted IDR values.}
\item{l}{The number of reproducible features at the specified \code{threshold}.}
\item{threshold}{The IDR threshold at which features are deemed reproducible.}
\item{Khat}{A vector signifying whether the corresponding feature is reproducible or not.}
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

set.seed(1123)

# True parameters
true.par <- c(0.9, 2, 0.7, 0.6)

# Simulation of data from the GMCM model
data <-  SimulateGMCMData(n = 1000, par = true.par, d = 2)

# Initial parameters
init.par <- c(0.5, 1, 0.5, 0.9) 

# Nelder-Mead optimization
nm.par   <- fit.meta.GMCM(data$u, init.par = init.par, method = "NM") 

# Get IDR values
res <- get.IDR(data$u, nm.par, threshold = 0.05)

# Plot results
plot(data$u, col = res$Khat, pch = c(3,16)[data$K])

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
