#  News for **GMCM**

# Version 1.4 (development)

 * AIC and BIC goodness of fit estimation is now available via the function
     `goodness.of.fit`.
 * Added a graphical interface  for the package via a shiny application.
     Available at [online](https://gmcm.shinyapps.io/GMCM/) or via `runGMCM()` for 
     a local instance.
 * Added `classify()` function to ease classification after fitting a general 
     GMCM.
 * Added vignettes and 
     [documentation website](https://AEBilgrau.github.io/GMCM/) via 
     [the pkgdown package](https://cran.r-project.org/package=pkgdown).


# Version 1.3.2 (2018-03-12)

 *  Hotfix due to new default RNG after R version 3.5.3.


# Version 1.3.1 (2018-02-14)

 *  By request, `EMAlgorithm` is now exported to the user interface with slight 
      tweaks. Better docs and examples are also provided.
 *  Expanded capability of `as.theta()` and fixed issue incorrect default axis 
      labels in `plot.theta`.
 *  Fixed broken link in vignette.
 *  Minor improvements to documentation.

# Version 1.3 (2018-12-05)

 *  Fixed issue with optim wrappers for the likelihood (`vector2theta` and 
      `theta2vector`) causing the first anchored component to be scaled 
      slightly incorrectly. Note, fitted results will be changed compared to 
      earlier versions (although hopefully not much).
 *  Functions now return a proper `S3` object of class `theta` and 
      `print.theta()`, `summary.theta()`, and `plot.theta()` functions have 
      been made to fit the usually expected generic functions.
 *  Minor improvements and corrections to documentation.
 *  Technical changes (with no impact on user interface):
    -  Updated package to use latest \cpkg{roxygen2} and \cpkg{Rcpp} packages.
    -  Updated [Travis-CI](https://travis-ci.org/AEBilgrau/GMCM) yaml file to
       'newly' supported R-language.
    -  Expanded unit tests and added continuous integration of test coverage with
       [coveralls.io](https://coveralls.io/github/AEBilgrau/GMCM?branch=master) 
       utilizing the \pkg{covr} package.

# Version 1.2.4 (2017-01-13 )

 *  Added vignette.
 *  Minor tweaks to documentation.
 *  Updated affiliations.
 *  Fixed broken links.


# Version 1.2.3 (2016-03-30)

 *  Updated citation information and manuscript.
 *  Minor updates to documentation.
 *  Patch due to changes in \pkg{testthat} and published manuscript.


# Version 1.2.2 (2015-08-05)

 *  Patch due to changes in \pkg{RcppArmadillo}. No visible user changes.


# Version 1.2.1 (2015-04-20)

 *  Hotfix due to failure of unit tests on sparc solaris. No visible user
      changes.


# Version 1.2 (2015-04-14)

 *  Added the GMCM package vignette. The article is to appear in the
      Journal of Statistical Software.
 *  `rtheta` has been updated substantially to allow for finer and
      more explicit control if needed. Default is the old behavior.
 *  Documentation has improved.


# Version 1.1.1 (2014-10-07)

 *  Fixed a small testing issue on CRAN for old R versions.
      (Used `anyNA` on R versions older than 3.1.0).


# Version 1.1 (2014-10-01)

 *  This NEWS.Rd file have been added!
 *  The \cpkg{GMCM}-package is now using the in-source documentation
      system \cpkg{roxygen2}.
 *  The `EStep` has been implemented in
      `Rcpp` and thus GMCM now features a faster pseudo EM algorithm.
 *  \cpkg{GMCM} is no longer dependent on the \cpkg{mvtnorm} package.
      \cpkg{GMCM} now features faster simulation of data.
 *  Functions `rmvnormal` and `dmvnormal` are now exported and
      visible to the user. These are now both implemented in \cpkg{Rcpp}
      and are faster versions of `rmvnorm` and
      `dmvnorm` from \cpkg{mvtnorm}.
 *  \cpkg{GMCM} is now using \cpkg{testthat} for automated
      testing. The automated test suite will further be expanded in the
      upcoming versions.
 *  Internal functions are now correctly hidden using the internal
      keyword.
 *  The order of function arguments `verbose` and `max.ite` in
      `fit.meta.GMCM` has been swapped to make it consistent with
      `fit.full.GMCM`. Apologies for any inconveniences.
 *  `get.idr` is no longer exported and visible to the user.
      Use `get.IDR` to get the local (posterior class probabilities)
      and adjusted irreproducibility discovery rates (IDR).
 *  Added [Travis Continuous Integration](https://travis-ci.org/AEBilgrau/GMCM) 
      for R building.


# Version 1.0.1 (2014-05-30)

 *  `pgmm_marginal` and other functions in `src/GMCM.cpp` have
      been rewritten to address memory issues. \cpkg{GMCM} should now be more
      stable and faster.


# Version 1.0 (2014-02-07)

 *  First version on CRAN.
