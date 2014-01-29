\name{GMCM-package}
\alias{GMCM-package}
\alias{GMCM}
\docType{package}
\title{
Fast evaluation of Gaussian Mixture Copula Models
}
\description{
Gaussian mixture copula models (GMCM) can be used for unsupervised clustering and meta analysis. In meta analysis, GMCMs are used to identify features which have been reproduced across multiple experiments. This package provides a fast and general implementation of GMCM cluster analysis and extension of the features available in the \code{idr} package.
}
\details{
\tabular{ll}{
Package: \tab GMCM\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-12-16\cr
License: \tab GPL (>=2)\cr
}

If the meta analysis of Li et al. (2011) is to be performed, the function \code{\link{fit.meta.GMCM}} is used to identify the maximum likelihood estimate of the special Gaussian mixture copula model (GMCM). The function \code{\link{get.IDR}} computes the local and adjusted Irreprodcible Discovery Rates defined by Li et al. (2011) to determine the level of reproduciblity.

If general unsupervised clustering is needed, like above, the function \code{\link{fit.full.GMCM}} computes the maximum likelihood estimate of the general GMCM. The function \code{\link{get.prob}} is used to estimate the class membership probabilities of each observation.

\code{\link{SimulateGMCMData}} provide easy simulation from the GMCMs.
}
\author{
Anders Ellern Bilgrau, Martin Boegsted, Poul Svante Eriksen

Maintainer: Anders Ellern Bilgrau <abilgrau@math.aau.dk>
}
\references{
Li, Q., Brown, J. B. J. B., Huang, H., & Bickel, P. J. (2011). Measuring reproducibility of high-throughput experiments. The Annals of Applied Statistics, 5(3), 1752-1779. doi:10.1214/11-AOAS466
}
%~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R ~~
%~~ documentation directory ~~
\keyword{ package }
\seealso{
Core user functions: \code{\link{fit.meta.GMCM}}, \code{\link{fit.full.GMCM}}, \code{\link{get.IDR}}, \code{\link{get.prob}}, \code{\link{SimulateGMCMData}}, \code{\link{SimulateGMMData}}, \code{\link{rtheta}}, \code{\link{Uhat}}, \code{\link{choose.theta}}, \code{\link{full2meta}}, \code{\link{meta2full}}

Package by Li et. al. (2011): \code{\link[idr:idr-package]{idr}}.
}
\examples{
# Loading ranked data
data(u133VsExon)

# Ranking and scaling,
# Remember large values should be critical to the null!
uhat <- Uhat(1 - u133VsExon)

# Visualizing P-values
plot(u133VsExon, cex = 0.5, pch = 4, col = "tomato", main = "P-values",
     xlab = "P   (U133)", ylab = "P   (Exon)")

# Visualizing Ranked and scaled P-values
plot(uhat, cex = 0.5, pch = 4, col = "tomato", main = "Ranked P-values",
     xlab = "rank(1-P)   (U133)", ylab = "rank(1-P)   (Exon)")

# Fitting using BFGS
fit1 <- fit.meta.GMCM(uhat, init.par = c(0.5, 1, 1, 0.5), pgtol = 1e-3,
                      method = "L-BFGS", positive.rho = TRUE, verbose = TRUE)

# Fitting using Nelder-Mead
# and a different reparametrization of rho: (rho can be negative)
fit2 <- fit.meta.GMCM(uhat, init.par = c(0.2, 2, 2, 0), reltol = 1e-3,
                      method = "NM", positive.rho = FALSE, verbose = TRUE)

# Both fits agree quite well
rbind(fit1, fit2)  

# Compute IDR values and classify
idr <- get.IDR(uhat, par = fit1)

table(idr$K) # 1 = irreproducible, 2 = reproducible
plot(uhat, cex = 0.5, pch = 4, main = "Classified genes",
     col = c("tomato", "steelblue")[idr$K],
     xlab = "rank(1-P)   (U133)", ylab = "rank(1-P)   (Exon)")
}