\name{PseudoEMAlgorithm}
\alias{PseudoEMAlgorithm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
EM-like algorithm for the GMCM
}
\description{
An fast and modified implementation of the Li et. al. (2011) EM-like algorithm for estimating the maximizing parameters of the GMCM-likelihood function.
}
\usage{
PseudoEMAlgorithm(x, theta, 
                  eps = 1e-4, 
                  max.ite = 1e3, 
                  verbose = FALSE, 
                  trace.theta = FALSE, 
                  meta.special.case = FALSE,
                  convergence.criterion = 
                    c("absGMCM","GMCM","GMM", "Li", "absLi"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A matrix of observations where rows corresponds to features and columns to experiments.
}
  \item{theta}{
A list of parameters formatted as described in \code{\link{rtheta}}.
}
  \item{eps}{
The maximum difference required to achive convergence. 
}
  \item{max.ite}{
The maximum number of iterations.
}
  \item{verbose}{
Logical. Set to \code{TRUE} to increase verbosity.
}
  \item{trace.theta}{
Logical. If \code{TRUE}, a trace of the estimated thetas are returned.
}
  \item{meta.special.case}{
Logical. If \code{TRUE}, the esimators used are for the special case GMCM of Li et. al. (2011).
}
  \item{convergence.criterion}{
Character. Sets the convergence criterion.  If \code{"absGMCM"} the absolute value of difference in GMCM is used. If \code{"GMCM"} the difference in GMCM-likelihoods are used as convergence criterion. If \code{"GMM"}, the guaranteed non-decreasing difference of GMM-likelihoods are used. If \code{"Li"}, the convergence criterion used by Li et. al. (2011) is used. If \code{"absLi"}, the absolute values of the Li et. al. criterion.
}
}
\details{
When either \code{"absGMCM"} or \code{"absLi"} are used, the parameters corresponding to the biggest observed likelihood is returned. This is not necessarily the last iteration.
}
\value{
A list of 3 or 4 is returned depending on the value of \code{trace.theta}
\item{theta}{A list containing the final parameter estimate in the format of \code{\link{rtheta}}}
\item{loglik.tr}{A matrix with different log-likelihood traces in each row.}
\item{kappa}{A matrix where the (i,j)'th entry is the probability that \code{x[i, ]} belongs to the j'th component. Usually the returned value of \code{EStep}.}
\item{theta.tr}{A list of each obtained parameter estimates in the format of \code{\link{rtheta}}}
}
\references{
Li, Q., Brown, J. B. J. B., Huang, H., & Bickel, P. J. (2011). Measuring reproducibility of high-throughput experiments. The Annals of Applied Statistics, 5(3), 1752-1779. doi:10.1214/11-AOAS466
}
\author{
Anders Ellern Bilgrau (abilgrau@math.aau.dk)
}
\note{
The algorithm is highly sensitive to the starting parameters which therefore should be carefully chosen.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{rtheta}}, \code{\link{EMAlgorithm}}
}
\examples{
set.seed(1)

# Choosing the true parameters and simulating data
true.par <- c(0.8, 3, 1.5, 0.4)
data <- SimulateGMCMData(n = 1000, par = true.par, d = 2)
uhat <- Uhat(data$u)  # Observed ranks

# Plot of latent and observed data colour coded by the true component
par(mfrow = c(1,2))
plot(data$z, main = "Latent data", cex = 0.6,
     xlab = "z (Experiment 1)", ylab = "z (Experiment 2)", 
     col = c("red","blue")[data$K])
plot(uhat, main = "Observed data", cex = 0.6,
     xlab = "u (Experiment 1)", ylab = "u (Experiment 2)", 
     col = c("red","blue")[data$K])

# Fit the model using the Pseudo EM algorithm
init.par <- c(0.5, 1, 1, 0.5)
res <- GMCM:::PseudoEMAlgorithm(uhat, meta2full(init.par, d = 2), 
                                verbose = TRUE,
                                convergence.criterion = "absGMCM", 
                                eps = 1e-4,
                                trace.theta = FALSE, 
                                meta.special.case = TRUE)

# Compute posterior cluster probabilities
IDRs <- get.IDR(uhat, par = full2meta(res$theta))

# Plot of observed data colour coded by the MAP estimate
plot(res$loglik[3,], main = "Loglikelihood trace", type = "l", 
     ylab = "log GMCM likelihood")
abline(v = which.max(res$loglik[3,])) # Chosen MLE
plot(uhat, main = "Clustering\nIDR < 0.05", xlab = "", ylab = "", cex = 0.6,
     col = c("Red","Blue")[IDRs$Khat])

# View parameters
rbind(init.par, true.par, estimate = full2meta(res$theta))

# Confusion matrix
table("Khat" = IDRs$Khat, "K" = data$K)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
