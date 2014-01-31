GMCM: Fast estimation of Gaussian Mixture Copula Models
====================================
The [GMCM package](http://cran.r-project.org/package=GMCM) offers R functions that perform high-dimensional meta-analysis and general unsupervised cluster analysis in a fast manner.

The package utilizes [Rcpp](https://github.com/RcppCore/Rcpp) and [RcppArmadillo](https://github.com/RcppCore/RcppArmadillo) to evaluate the likelihood function quickly.

Additional information and documentation will follow very soon. For now, see ‘help(“GMCM")' for some help and examples.

## Installation

The released and tested version of GMCM is available at
[CRAN](http://cran.r-project.org) (Comprehensive R Archive Network). It can be easily be installed from within R by running 

```R
install.packages(“GMCM”)
```

If you wish to install the latest GMCM version directly from the master branch at GitHub, run 

```R
install.packages("devtools")
devtools::install_github(“AEBilgrau/GMCM”)
```

If you install from the source, you need to have the 
[package development prerequisites](http://www.rstudio.com/ide/docs/packages/prerequisites).

