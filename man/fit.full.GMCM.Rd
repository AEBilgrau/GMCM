\name{fit.full.GMCM}
\alias{fit.full.GMCM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Maximum likelihood estimate of the GMCM of Tewari et. al. (2011).
}
\description{
Various optimization procedures to find the maximum likelihood estimate of a Gaussian mixture copula model [Tewari et al. (2011)].
}
\usage{
fit.full.GMCM(u, 
              m,
              theta = choose.theta(u, m),
              method = c("NM", "SANN", "L-BFGS", "L-BFGS-B", "PEM"),
              max.ite = 1000, 
              verbose = TRUE,
              ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{u}{
An n by d matrix of ranked and scaled test statistics. Rows correspond to observations and columns to the dimensions of the variables.
}
  \item{m}{
The number of components to be fitted.
}
  \item{theta}{
A list of parameters as defined in \code{\link{rtheta}}. If \code{theta} is not provided, then heuristic starting values are chosen using the k-means algorithm.
}
  \item{method}{
A character vector of length \eqn{1}{1}. The optimization method used. Should be either \code{"NM"}, \code{"SANN"}, \code{"L-BFGS"}, \code{"L-BFGS-B"}, or \code{"PEM"} which are the Nelder-Mead, Simulated Annealing, limited-memory quasi-Newton method, limited-memory quasi-Newton method with box constraints, and the pseudo EM algorithm, respectively. Default is \code{"NM"}.
See \code{\link{optim}} for further details.
}
  \item{max.ite}{
The maximum number of iterations. If the \code{method} is \code{"SANN"} this is the number of interations as there is no other stopping criterion. (See \code{\link{optim}})
}
  \item{verbose}{
Logical. If \code{TRUE}, a trace of the parameter estimates is made.
}
\item{\dots}{
Arguments passed to the \code{control}-list in \code{\link{optim}} or \code{\link{PseudoEMAlgorithm}} if the \code{method} is \code{"PEM"}.
}
}
\details{
The \code{"L-BFGS-B"} method does not perform a transformation of the parameters and uses box-contraints as implemented in \code{optim}.

Note that the many parameter configurations are poorly estimable or directly unidentifiable.
}
\value{
A list of parameters formatted as described in \code{\link{rtheta}}.
}
\references{
Li, Q., Brown, J. B. J. B., Huang, H., & Bickel, P. J. (2011). Measuring reproducibility of high-throughput experiments. The Annals of Applied Statistics, 5(3), 1752-1779. doi:10.1214/11-AOAS466

Tewari, A., Giering, M. J., & Raghunathan, A. (2011). Parametric Characterization of Multimodal Distributions with Non-gaussian Modes. 2011 IEEE 11th International Conference on Data Mining Workshops, 286-292. doi:10.1109/ICDMW.2011.135
}
\author{
Anders Ellern Bilgrau (abilgrau@math.aau.dk)
}
\note{
All the optimization procedures are stongly dependent on the initial values and the cooling scheme. Therefore it is advisable to apply multiple different initial parameters and select the best fit.

The \code{\link{choose.theta}} itself chooses random initializations. Hence, the output when \code{theta} is not directly supplied can vary.

See \code{\link{optim}} for further details.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{optim}}, \code{\link{get.prob}}
}
\examples{

set.seed(17)
sim <- SimulateGMCMData(n = 1000, m = 3, d = 2)

# Plotting simulated data
par(mfrow = c(1,2))
plot(sim$z, col = rainbow(3)[sim$K], main = "Latent process")
plot(sim$u, col = rainbow(3)[sim$K], main = "GMCM process")

# Observed data
uhat <- Uhat(sim$u) 

# The model should be fitted multiple times using different starting estimates
start.theta <- choose.theta(uhat, m = 3)  # Random starting estimate
res <- fit.full.GMCM(u = uhat, theta = start.theta, 
                     method = "NM", max.ite = 3000,
                     reltol = 1e-2, trace = TRUE)  # Note, 1e-2 is too big

# Confusion matrix
Khat <- apply(get.prob(uhat, theta = res), 1, which.max)
table("Khat" = Khat, "K" = sim$K)  # Note, some components have been swapped

# Simulation from GMCM with the fitted parameters
simfit <- SimulateGMCMData(n = 1000, theta = res)

# As seen, the underlying latent process is hard to estimate.
# The clustering, however, is very good.
par(mfrow = c(2,2))
plot(simfit$z, col = simfit$K, main = "Model check 1\nSimulated GMM")
plot(simfit$u, col = simfit$K, main = "Model check 2\nSimulated GMCM")
plot(sim$u, col = Khat, main = "MAP clustering")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
