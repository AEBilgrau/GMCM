
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Build Status](https://api.travis-ci.org/AEBilgrau/GMCM.svg?branch=master)](https://travis-ci.org/AEBilgrau/GMCM)
[![CRAN version](http://www.r-pkg.org/badges/version/GMCM)](https://cran.r-project.org/package=GMCM)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/GMCM)](https://cran.r-project.org/package=GMCM)
[![Total CRAN downloads](http://cranlogs.r-pkg.org/badges/grand-total/GMCM)](https://cran.r-project.org/package=GMCM)
[![Coverage Status](https://coveralls.io/repos/github/AEBilgrau/GMCM/badge.svg?branch=master)](https://coveralls.io/github/AEBilgrau/GMCM?branch=master)

GMCM
----
### Fast estimation of Gaussian Mixture Copula Models

The [**GMCM** package](https://cran.r-project.org/package=GMCM) 
([Bilgrau et. al., 2016](https://www.jstatsoft.org/article/view/v070i02))
offers R functions that very fast perform high-dimensional meta-analysis 
[(Li et. al., 2011)](http://arxiv.org/pdf/1110.4705.pdf) 
and general unsupervised cluster analysis 
[(Tewari et. al., 2011)](http://ieeexplore.ieee.org/xpl/articleDetails.jsp?arnumber=6137392) 
using Gaussian Copula Mixture Models. 
Online documentation is available [here.](http://AEBilgrau.github.io/GMCM)

Gaussian copula mixture models (GMCMs) are a very flexible alternative to regular Gaussian mixture models (GMMs) in unsupervised cluster analysis of continuous data where non-normal clusters are present. 
GMCMs models the *ranks* of the observed data and are thus invariant to monotone increasing transformations of the data, i.e. they are semi-parametric and only the ordering of the data is important providing needed flexibility. 
A special-case of GMCMs can be used for a novel meta-analysis approach in high-dimensional settings. 
In this context, the model tries to cluster results into two groups which agree and do not agree on statistical evidence. These two groups corresponds to a reproducible and irreproducible group.

The optimization of the complicated likelihood function is difficult, however. 
**GMCM** utilizes 
[**Rcpp**](https://github.com/RcppCore/Rcpp) 
and 
[**RcppArmadillo**](https://github.com/RcppCore/RcppArmadillo) 
to evaluate the likelihood function quickly and arrive at a parameter estimate using either standard numerical optimization routines or an pseudo EM algorithm.

Additional information, documentation, help, and examples can be found by [here](https://aebilgrau.github.io/GMCM
) or by running `?GMCM` in **R**. 
The paper[1] is also found as a vignette by `vignette("GMCM-JStatSoft")`  or [the official website online.](https://www.jstatsoft.org/article/view/v070i02).
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

When installed, run `GMCM::runGMCM()` to launch a local instance of the GMCM shiny application also available [online at shinyapps.io](https://gmcm.shinyapps.io/GMCM/).
Run `news(package = "GMCM")` to view the latest changes of GMCM or visit [here](http://AEBilgrau.github.io/GMCM/news).

For previous versions of **GMCM**, visit the old [releases at GitHub](https://github.com/AEBilgrau/GMCM/releases) or the [archive at CRAN.](https://cran.r-project.org/src/contrib/Archive/GMCM/)


## Usage
As noted above, the usage of GMCM comes in two different applications; one general and one special.

#### Special GMCMs
An example of using the package to fit special GMCMs for meta analysis of is described here `vignette("usage-example-special-model")`. This model is a specific special case of the general GMCMs. 

#### General GMCMs
An example of unsupervised clustering using the package is found with `vignette("usage-example-general-model")` for general purposes. 

#### Shiny
The package also provides a graphical user interface via Shiny for both its uses. See
`vignette("usage-shiny-graphical-interface")`.


## References

  1. Anders Ellern Bilgrau, Poul Svante Eriksen, Jakob Gulddahl Rasmussen, Hans 
     Erik Johnsen, Karen Dybkaer, Martin Boegsted (2016). **GMCM: Unsupervised 
     Clustering and Meta-Analysis Using Gaussian Mixture Copula Models.** 
     Journal of Statistical Software, 70(2), 1-23. [doi:10.18637/jss.v070.i02](http://doi.org/10.18637/jss.v070.i02)

---
