\name{EMAlgorithm}
\alias{EMAlgorithm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 EM algorithm for Gaussian mixture models
}
\description{
The regular expectation-maximization algorithm for general multivariate Gaussian mixture models.
}
\usage{
EMAlgorithm(x, theta, 
            eps = 10^-6, max.ite = 10^5, 
            trace.theta = FALSE, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A matrix of observations where rows correspond to features and columns to experiments.
}
  \item{theta}{
A list of parameters as described in \code{\link{rtheta}}.
}
  \item{eps}{
The maximal required difference in sucessive likelihoods to establish convergence.
}
  \item{max.ite}{
The maximum number of interations.
}
  \item{trace.theta}{
Logical. If \code{TRUE}, all estimates are stored and returned. Default is \code{FALSE}.
}
  \item{verbose}{
Set to \code{TRUE} for verbose output. Default is \code{FALSE}.
}
}
\details{
Though not as versatile, the algorithm can be a faster alternative to \code{Mclust} in the \code{mclust}-package.
}
\value{
A list of length 3 with elements:
\item{theta }{A list of the estimated parameters as described in \code{\link{rtheta}}.}
\item{loglik.tr }{A numeric vector of the log-likelihood trace.}
\item{kappa }{A matrix where \code{kappa[i,j]} is the probability that \code{x[i, ]} is realized from the \code{j}'th component.}
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
\code{\link{rtheta}}, \code{\link{PseudoEMAlgorithm}}
}
\examples{
set.seed(10)
data <- SimulateGMCMData(n = 1000, d = 2, m = 3)
start.theta <- rtheta(d = 2, m = 3)
res <- GMCM:::EMAlgorithm(data$z, theta = start.theta)

par(mfrow = c(1,2))
plot(data$z, cex = 0.5, pch = 16, main = "Simulated data",
     col = rainbow(3)[data$K])
plot(data$z, cex = 0.5, pch = 16, main = "GMM clustering",
     col = rainbow(3)[apply(res$kappa,1,which.max)])
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
