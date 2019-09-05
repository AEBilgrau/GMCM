[![Build Status](https://api.travis-ci.org/AEBilgrau/GMCM.svg?branch=master)](https://travis-ci.org/AEBilgrau/GMCM)
[![CRAN version](http://www.r-pkg.org/badges/version/GMCM)](https://cran.r-project.org/package=GMCM)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/GMCM)](https://cran.r-project.org/package=GMCM)
[![Total CRAN downloads](http://cranlogs.r-pkg.org/badges/grand-total/GMCM)](https://cran.r-project.org/package=GMCM)
[![Coverage Status](https://coveralls.io/repos/github/AEBilgrau/GMCM/badge.svg?branch=master)](https://coveralls.io/github/AEBilgrau/GMCM?branch=master)

GMCM
----
### Fast estimation of Gaussian Mixture Copula Models

The [**GMCM** package](https://cran.r-project.org/package=GMCM) 
offers R functions that perform high-dimensional meta-analysis 
[(Li et. al., 2011)](http://arxiv.org/pdf/1110.4705.pdf) 
and general unsupervised cluster analysis 
[(Tewari et. al., 2011)](http://ieeexplore.ieee.org/xpl/articleDetails.jsp?arnumber=6137392) 
using Gaussian Copula Mixture Models in a very fast manner [[1](https://www.jstatsoft.org/article/view/v070i02)].

Gaussian copula mixture models (GMCMs) are a very flexible alternative to Gaussian mixture models in unsupervised cluster analysis for continuous data where non-Gaussian clusters are present. 
GMCMs model the ranks of the observed data and are thus invariant to monotone increasing transformations of the data, i.e. they are semi-parametric and only the ordering of the data is important. 
Alternatively, a special-case of the GMCMs can be used for a novel meta-analysis approach in high-dimensional settings. 
In this context, the model tries to cluster results which agree and do not agree on statistical evidence into a reproducible and irreproducible group.

The optimization of the complicated likelihood function is difficult, however. 
**GMCM** utilizes 
[**Rcpp**](https://github.com/RcppCore/Rcpp) 
and 
[**RcppArmadillo**](https://github.com/RcppCore/RcppArmadillo) 
to evaluate the likelihood function quickly and arrive at a parameter estimate using either standard numerical optimization routines or an pseudo EM algorithm.

Additional information, documentation, help, and examples can be found by running `help(package = "GMCM")`  or `?GMCM` in **R**. 
The paper [1] is also found as a vignette by `vignette("GMCM-JStatSoft")`.
The core user functions of **GMCM** are `fit.full.GMCM` and `fit.meta.GMCM`.

## Installation

The released and tested version of **GMCM** is available at
[CRAN](https://cran.r-project.org/package=GMCM) 
(Comprehensive R Archive Network).
It can be installed from within R by running 

```R
install.packages("GMCM")
```

If you wish to install the latest version of **GMCM** directly from the master branch at GitHub, run 

```R
#install.packages("remotes")  # Install remotes if needed
remotes::install_github("AEBilgrau/GMCM")
```

Note, that this version is in development and is likely different from the version at CRAN. 
As such, it may be unstable. Be sure that you have the 
[package development prerequisites](https://support.rstudio.com/hc/en-us/articles/200486498-Package-Development-Prerequisites) 
if you wish to install the package from the source.


When installed, run `GMCM::runGMCM()` to launch a local instance of the GMCM shiny application also available online [shinyapps.io](https://gmcm.shinyapps.io/GMCM/)).
Run `news(package = "GMCM")` to view the latest changes of GMCM.

For previous versions of **GMCM**, visit the old [releases at GitHub](https://github.com/AEBilgrau/GMCM/releases) or the [archive at CRAN.](https://cran.r-project.org/src/contrib/Archive/GMCM/)

## References

  1. Anders Ellern Bilgrau, Poul Svante Eriksen, Jakob Gulddahl Rasmussen, Hans 
     Erik Johnsen, Karen Dybkaer, Martin Boegsted (2016). **GMCM: Unsupervised 
     Clustering and Meta-Analysis Using Gaussian Mixture Copula Models.** 
     Journal of Statistical Software, 70(2), 1-23. [doi:10.18637/jss.v070.i02](https://www.jstatsoft.org/article/view/v070i02)

---
