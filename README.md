GMCM: Fast estimation of Gaussian Mixture Copula Models
-------------------------------------------------------

The [GMCM package](http://cran.r-project.org/package=GMCM) offers R functions that perform high-dimensional meta-analysis [(Li et. al., 2011)](http://arxiv.org/pdf/1110.4705.pdf) and general unsupervised cluster analysis [(Tewari et. al., 2011)](http://ieeexplore.ieee.org/xpl/articleDetails.jsp?arnumber=6137392) using Gaussian Copula Mixture Models in a vey fast manner.

The package utilizes [Rcpp](https://github.com/RcppCore/Rcpp) and [RcppArmadillo](https://github.com/RcppCore/RcppArmadillo) to evaluate the likelihood function quickly.

Additional information and documentation will follow very soon. For now, see `help("GMCM")` for some help and examples.

## Installation

The released and tested version of GMCM is available at
[CRAN](http://cran.r-project.org/package=GMCM) (Comprehensive R Archive Network). It can be easily be installed from within R by running 

```R
install.packages("GMCM")
```

If you wish to install the latest version of GMCM directly from the master branch at GitHub, run 

```R
install.packages("devtools")
devtools::install_github("AEBilgrau/GMCM")
```

Note, that this version is in development and different from the CRAN version and may be unstable. Be sure that you have the 
[package development prerequisites](http://www.rstudio.com/ide/docs/packages/prerequisites) if you install from the source.

---
