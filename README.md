GMCM: Fast estimation of Gaussian Mixture Copula Models
-------------------------------------------------------

The [GMCM package](http://cran.r-project.org/package=GMCM) offers R functions that perform high-dimensional meta-analysis [(Li et. al., 2011)](http://arxiv.org/pdf/1110.4705.pdf) and general unsupervised cluster analysis [(Tewari et. al., 2011)](http://ieeexplore.ieee.org/xpl/articleDetails.jsp?arnumber=6137392) using Gaussian Copula Mixture Models in a very fast manner.

Gaussian copula mixture models (GMCMs) are a very flexible alternative to gaussian mixture models in unsupervised cluster analysis for continuous data where non-normally distributed clusters are present. GMCMs model the ranks of the observed data and are thus invariant to monotone increasing transformations of the data, i.e. the only the ordering of the data is important. Alternatively, a special-case of the GMCMs can be used for a novel meta-analysis approach in high-dimensional settings. In this context, the model tries to cluster results which agree and don't agree on statistical evidence into a reproducible and irreproducible group.

The optimization of the complicated likelihood function is difficult, however. The GMCM package utilizes [Rcpp](https://github.com/RcppCore/Rcpp) and [RcppArmadillo](https://github.com/RcppCore/RcppArmadillo) to evaluate the likelihood function quickly.

Additional information and documentation will follow very soon. For now, run `help("GMCM")` in R for some help and examples.

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

Note, that this version is in development and different from the CRAN version. As such, it may be unstable. Be sure that you have the 
[package development prerequisites](http://www.rstudio.com/ide/docs/packages/prerequisites) if you wish to install the package from the source.

---
