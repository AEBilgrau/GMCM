% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit.full.GMCM.R
\name{fit.full.GMCM}
\alias{fit.full.GMCM}
\alias{fit.full.gmcm}
\alias{fit.general.GMCM}
\alias{fit.general.gmcm}
\title{Estimate GMCM parameters of the general model}
\usage{
fit.full.GMCM(
  u,
  m,
  theta = choose.theta(u, m),
  method = c("NM", "SANN", "L-BFGS", "L-BFGS-B", "PEM"),
  max.ite = 1000,
  verbose = TRUE,
  ...
)

fit.general.GMCM(
  u,
  m,
  theta = choose.theta(u, m),
  method = c("NM", "SANN", "L-BFGS", "L-BFGS-B", "PEM"),
  max.ite = 1000,
  verbose = TRUE,
  ...
)
}
\arguments{
\item{u}{An \code{n} by \code{d} matrix of marginally uniform observations.
Rows corresponds to observations and columns to the dimensions of the
variables. I.e. these are often ranked and scaled test statistics or other
observations.}

\item{m}{The number of components to be fitted.}

\item{theta}{A list of parameters as defined in \code{\link{rtheta}}. If
\code{theta} is not provided, then heuristic starting values are chosen
using the k-means algorithm.}

\item{method}{A character vector of length \eqn{1}{1}. The optimization
method used. Should be either \code{"NM"}, \code{"SANN"}, \code{"L-BFGS"},
\code{"L-BFGS-B"}, or \code{"PEM"} which are the Nelder-Mead, Simulated
Annealing, limited-memory quasi-Newton method, limited-memory quasi-Newton
method with box constraints, and the pseudo EM algorithm, respectively.
Default is \code{"NM"}. See \code{\link{optim}} for further details.}

\item{max.ite}{The maximum number of iterations. If the \code{method} is
\code{"SANN"} this is the number of iterations as there is no other
stopping criterion. (See \code{\link{optim}})}

\item{verbose}{Logical. If \code{TRUE}, a trace of the parameter estimates
is made.}

\item{\dots}{Arguments passed to the \code{control}-list in
\code{\link{optim}} when \code{method} is not equal to \code{"PEM"}.
If \code{method} equals \code{"PEM"}, the arguments are passed to
 \code{\link{PseudoEMAlgorithm}} if the \code{method}.}
}
\value{
A list of parameters formatted as described in \code{\link{rtheta}}.

When \code{method} equals \code{"PEM"}, a list of extra information
(log-likelihood trace, the matrix of group probabilities, theta trace) is
added as an attribute called "extra".
}
\description{
Estimates the parameters of general Gaussian mixture copula models (GMCM).
The function finds the maximum likelihood estimate of a general
GMCM with various optimization procedures. Note, all but the PEM methods
provides the maximum likelihood estimate.
}
\details{
The \code{"L-BFGS-B"} method does not perform a transformation of
  the parameters and uses box constraints as implemented in \code{optim}. \cr
  Note that the many parameter configurations are poorly estimable or
  directly unidentifiable.

  \code{fit.general.GMCM} is simply an alias of \code{fit.full.gmcm}.
}
\note{
All the optimization procedures are strongly dependent on the initial
  values and other parameters (such as the cooling scheme for method SANN).
  Therefore it is advisable to apply multiple
  different initial parameters (and optimization routines) and select the
  best fit.

  The \code{\link{choose.theta}} itself chooses random a initialization.
  Hence, the output when \code{theta} is not directly supplied can vary.

  See \code{\link{optim}} for further details.
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
\references{
Li, Q., Brown, J. B. J. B., Huang, H., & Bickel, P. J. (2011).
  Measuring reproducibility of high-throughput experiments. The Annals of
  Applied Statistics, 5(3), 1752-1779. doi:10.1214/11-AOAS466

  Tewari, A., Giering, M. J., & Raghunathan, A. (2011). Parametric
  Characterization of Multimodal Distributions with Non-gaussian Modes. 2011
  IEEE 11th International Conference on Data Mining Workshops, 286-292.
doi:10.1109/ICDMW.2011.135
}
\seealso{
\code{\link{optim}}, \code{\link{get.prob}}
}
\author{
Anders Ellern Bilgrau <anders.ellern.bilgrau@gmail.com>
}
