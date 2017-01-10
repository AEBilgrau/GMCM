[![Build Status](https://api.travis-ci.org/AEBilgrau/GMCM.svg?branch=master)](https://travis-ci.org/AEBilgrau/GMCM)
[![CRAN version](http://www.r-pkg.org/badges/version/GMCM)](https://cran.r-project.org/package=GMCM)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/GMCM)](https://cran.r-project.org/package=GMCM)
[![Total CRAN downloads](http://cranlogs.r-pkg.org/badges/grand-total/GMCM)](https://cran.r-project.org/package=GMCM)

GMCM
----
### Fast estimation of Gaussian Mixture Copula Models

The [**GMCM** package](https://cran.r-project.org/package=GMCM) 
offers R functions that perform high-dimensional meta-analysis 
[(Li et. al., 2011)](http://arxiv.org/pdf/1110.4705.pdf) 
and general unsupervised cluster analysis 
[(Tewari et. al., 2011)](http://ieeexplore.ieee.org/xpl/articleDetails.jsp?arnumber=6137392) 
using Gaussian Copula Mixture Models in a very fast manner.

Gaussian copula mixture models (GMCMs) are a very flexible alternative to Gaussian mixture models in unsupervised cluster analysis for continuous data where non-Gaussian clusters are present. 
GMCMs model the ranks of the observed data and are thus invariant to monotone increasing transformations of the data, i.e. they are semi-parametric and only the ordering of the data is important. 
Alternatively, a special-case of the GMCMs can be used for a novel meta-analysis approach in high-dimensional settings. 
In this context, the model tries to cluster results which agree and don't agree on statistical evidence into a reproducible and irreproducible group.

The optimization of the complicated likelihood function is difficult, however. 
The **GMCM** package utilizes 
[**Rcpp**](https://github.com/RcppCore/Rcpp) 
and 
[**RcppArmadillo**](https://github.com/RcppCore/RcppArmadillo) 
to evaluate the likelihood function quickly and arrive at a parameter estimate using various standard numerical optimization routines.

Additional information, documentation, help, and examples can be found by running `help(package = "GMCM")`  or `?GMCM` in **R**. 
The paper [1] is also found as a vignette by `vignette("GMCM-JStatSoft")`.
The core user functions of **GMCM** are `fit.full.GMCM` and `fit.meta.GMCM`.

## Installation

The released and tested version of **GMCM** is available at
[CRAN](https://cran.r-project.org/package=GMCM) 
(Comprehensive R Archive Network).
It can be easily be installed from within R by running 

```R
install.packages("GMCM")
```

If you wish to install the latest version of **GMCM** directly from the master branch here at GitHub, run 

```R
#install.packages("devtools")
devtools::install_github("AEBilgrau/GMCM")
```

Note, that this version is in development and is different from the version at CRAN. 
As such, it may be unstable. Be sure that you have the 
[package development prerequisites](https://support.rstudio.com/hc/en-us/articles/200486498-Package-Development-Prerequisites) 
if you wish to install the package from the source.

When installed, run `news(package = "GMCM")` to view the latest notable changes of GMCM.

For previous versions of **GMCM**, visit the old [releases at GitHub](https://github.com/AEBilgrau/GMCM/releases) or the [archive at CRAN.](https://cran.r-project.org/src/contrib/Archive/GMCM/)

## References

  1. Anders Ellern Bilgrau, Poul Svante Eriksen, Jakob Gulddahl Rasmussen, Hans 
     Erik Johnsen, Karen Dybkaer, Martin Boegsted (2016). **GMCM: Unsupervised 
     Clustering and Meta-Analysis Using Gaussian Mixture Copula Models.** 
     Journal of Statistical Software, 70(2), 1-23. [doi:10.18637/jss.v070.i02](https://www.jstatsoft.org/article/view/v070i02)

## Usage
### Meta Analysis example
This is a very short tutorial for using the special GMCM for meta analysis. 
To illustrate we load the `u133VsExon` dataset within the package.
The dataset contains 19,577 *p*-values for the null hypothesis of no differential gene expression between two cell types for each of two different experiments called `u133` and `exon`.
```R
# Load and show data
data(u133VsExon)
head(u133VsExon, n = 3)
#                        u133         exon
#ENSG00000265096 1.756104e-01 1.072572e-01
#ENSG00000152495 1.779757e-03 6.741108e-10
#ENSG00000198040 5.370574e-03 1.505019e-03
```
See e.g. `?u133VsExon` for more information.
Next, we subset the data, rank it, and visualize it:
```R
# Subsetting data to reduce computation time
u133VsExon <- u133VsExon[1:5000, ]

# Ranking and scaling, remember large values should be critical to the null!
uhat <- Uhat(1 - u133VsExon)

par(mfrow = c(1,2))  # Visualizing P-values and the ranked P-values
plot(u133VsExon, cex = 0.5, pch = 4, col = "tomato", main = "P-values",
     xlab = "P-value (U133)", ylab = "P-value (Exon)")
plot(uhat, cex = 0.5, pch = 4, col = "tomato", main = "Ranked P-values",
     xlab = "rank(1-P) (U133)", ylab = "rank(1-P) (Exon)")
```
Here each point represent a gene. 
The genes in the lower left of the first panel and correspondingly in the upper right of the second panel are the seemingly reproducible genes.
They have a low *p*-value and thus a high rank and *both* experiments.
Next, we do the actual fit using the core user function `fit.meta.GMCM` with a L-BFGS-like procedure and subsequently compute the IDR values:
```R
fit <- fit.meta.GMCM(uhat, init.par = c(0.5, 1, 1, 0.5), 
                     method = "L-BFGS", pgtol = 1e-2, verbose = TRUE)
                     
idr <- get.IDR(uhat, par = fit)  # Compute IDR values and classify
table(idr$K)  # Show results, 1 = irreproducible, 2 = reproducible
#   1    2 
#4136  864 
````
Where we see that 864 of the 5000 genes are deemed reproducible by the model at the default 0.05 threshold.
The clustering results and the reproducible genes can be visualized in the following manner:
```R
par(mfrow = c(1,2))
plot(u133VsExon, cex = 0.5, pch = 4, main = "Classified genes",
     col = c("tomato", "steelblue")[idr$K],
     xlab = "P-value (U133)", ylab = "P-value (Exon)")
plot(uhat, cex = 0.5, pch = 4, main = "Classified genes",
     col = c("tomato", "steelblue")[idr$K],
     xlab = "rank(1-P) (U133)", ylab = "rank(1-P) (Exon)")
````
The model indeed capture genes in the upper right (of the second panel) and classify them as reproducible.

---
