\name{NEWS}
\title{News for Package \pkg{GMCM}}
\newcommand{\cpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\section{Changes in version 1.2.3}{
  \itemize{
    \item Updated citation information and manuscript.
    \item Minor updates to documentation.
    \item Patch due to changes in \pkg{testthat} and published manuscript.
  }
}

\section{Changes in version 1.2.2}{
  \itemize{
    \item Patch due to changes in \pkg{RcppArmadillo}. No visible user changes.
  }
}


\section{Changes in version 1.2.1}{
  \itemize{
    \item Hotfix due to failure of unit tests on sparc solaris. No visible user
      changes.
  }
}


\section{Changes in version 1.2}{
  \itemize{
    \item Added the GMCM package vignette. The article is to appear in the
      Journal of Statistical Software.
    \item \code{rtheta} has been updated substantially to allow for finer and
      more explicit control if needed. Default is the old behavior.
    \item Documentation has improved.
  }
}

\section{Changes in version 1.1.1}{
  \itemize{
    \item Fixed a small testing issue on CRAN for old R versions.
      (Used \code{anyNA} on R versions older than 3.1.0).
  }
}

\section{Changes in version 1.1}{
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
  }
}

\section{Changes in version 1.0.1}{
  \itemize{
    \item \code{pgmm_marginal} and other functions in \code{src/GMCM.cpp} have
      been rewritten to address memory issues. \cpkg{GMCM} should now be more
      stable and faster.
  }
}



