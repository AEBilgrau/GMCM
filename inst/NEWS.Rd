\name{NEWS}
\title{News for Package \pkg{GMCM}}
\newcommand{\cpkg}{\href{https://cran.r-project.org/package=#1}{\pkg{#1}}}


\section{Changes in version 1.4}{
  \itemize{
    \item Added a shiny application for the package. Available at \href{https://gmcm.shinyapps.io/GMCM/}{online} or via \code{GMCM::runGMCM()} for a local instance.
    \item Added \code{classify()} function to ease classification after fitting a general GMCM.
  }
}

\section{Changes in version 1.3.2 (2018-03-12)}{
  \itemize{
    \item Hotfix due to new default RNG after R version 3.5.3.
  }
}

\section{Changes in version 1.3.1 (2018-02-14)}{
  \itemize{
    \item By request, \code{EMAlgorithm} is now exported to the user interface with slight tweaks.
      Better docs and examples are also provided.
    \item Expanded capability of `as.theta()` and fixed issue incorrect default axis labels in `plot.theta`.
    \item Fixed broken link in vignette.
    \item Minor improvements to documentation.
  }
}

\section{Changes in version 1.3 (2018-12-05)}{
  \itemize{
    \item Fixed issue with optim wrappers for the likelihood (\code{vector2theta} and \code{theta2vector}) causing the first anchored component to be scaled slightly incorrectly. Note, fitted results will be changed compared to earlier versions (although hopefully not much).
    \item Functions now return a proper \code{S3} object of class \code{\link{theta}} and \code{\link{print.theta}}, \code{\link{summary.theta}}, and \code{\link{plot.theta}} functions have been made to fit the usually expected generic functions.
    \item Minor improvements and corrections to documentation.
    \item Technical changes (with no impact on user interface):
    \itemize{
      \item Updated package to use latest \cpkg{roxygen2} and \cpkg{Rcpp} packages.
      \item Updated \href{https://travis-ci.org/AEBilgrau/GMCM}{Travis-CI} yaml file to 'newly' supported R-language.
      \item Expanded unit tests and added continuous integration of test coverage with \href{https://coveralls.io/github/AEBilgrau/GMCM?branch=master}{coveralls.io} utilizing the \pkg{covr} package.
    }
  }
}

\section{Changes in version 1.2.4 (2017-01-13 )}{
  \itemize{
    \item Added vignette.
    \item Minor tweaks to documentation.
    \item Updated affiliations.
    \item Fixed broken links.
  }
}

\section{Changes in version 1.2.3 (2016-03-30)}{
  \itemize{
    \item Updated citation information and manuscript.
    \item Minor updates to documentation.
    \item Patch due to changes in \pkg{testthat} and published manuscript.
  }
}

\section{Changes in version 1.2.2 (2015-08-05)}{
  \itemize{
    \item Patch due to changes in \pkg{RcppArmadillo}. No visible user changes.

  }
}


\section{Changes in version 1.2.1 (2015-04-20)}{
  \itemize{
    \item Hotfix due to failure of unit tests on sparc solaris. No visible user
      changes.
  }
}


\section{Changes in version 1.2 (2015-04-14)}{
  \itemize{
    \item Added the GMCM package vignette. The article is to appear in the
      Journal of Statistical Software.
    \item \code{rtheta} has been updated substantially to allow for finer and
      more explicit control if needed. Default is the old behavior.
    \item Documentation has improved.
  }
}

\section{Changes in version 1.1.1 (2014-10-07)}{
  \itemize{
    \item Fixed a small testing issue on CRAN for old R versions.
      (Used \code{anyNA} on R versions older than 3.1.0).
  }
}

\section{Changes in version 1.1 (2014-10-01)}{
  \itemize{
    \item This NEWS.Rd file have been added!
    \item The \cpkg{GMCM}-package is now using the in-source documentation
      system \cpkg{roxygen2}.
    \item The \code{EStep} has been implemented in
      \code{Rcpp} and thus GMCM now features a faster pseudo EM algorithm.
    \item \cpkg{GMCM} is no longer dependent on the \cpkg{mvtnorm} package.
      \cpkg{GMCM} now features faster simulation of data.
    \item Functions \code{rmvnormal} and \code{dmvnormal} are now exported and
      visible to the user. These are now both implemented in \cpkg{Rcpp}
      and are faster versions of \code{rmvnorm} and
      \code{dmvnorm} from \cpkg{mvtnorm}.
    \item \cpkg{GMCM} is now using \cpkg{testthat} for automated
      testing. The automated test suite will further be expanded in the
      upcoming versions.
    \item Internal functions are now correctly hidden using the internal
      keyword.
    \item The order of function arguments \code{verbose} and \code{max.ite} in
      \code{fit.meta.GMCM} has been swapped to make it consistent with
      \code{fit.full.GMCM}. Apologies for any inconveniences.
    \item \code{get.idr} is no longer exported and visible to the user.
      Use \code{get.IDR} to get the local (posterior class probabilities)
      and adjusted irreproducibility discovery rates (IDR).
    \item Added \href{https://travis-ci.org/AEBilgrau/GMCM}{Travis Continuous Integration} for R building.
  }
}

\section{Changes in version 1.0.1 (2014-05-30)}{
  \itemize{
    \item \code{pgmm_marginal} and other functions in \code{src/GMCM.cpp} have
      been rewritten to address memory issues. \cpkg{GMCM} should now be more
      stable and faster.
  }
}

\section{Changes in version 1.0 (2014-02-07)}{
  \itemize{
    \item First version on CRAN.
  }
}
