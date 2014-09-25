\name{NEWS}
\title{News for Package \pkg{GMCM}}
\newcommand{\cpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

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
      visible to the user. These are now both implemented in Rcpp
      and are faster versions of \code{rmvnorm} and
      \code{dmvnorm} from \cpkg{mvtnorm}.
    \item \cpkg{GMCM} is now using \cpkg{testthat} for automated
      testing. The automated test suite will further be expanded in the
      upcoming versions.
    \item Internal functions have now correctly hidden using the internal
      keyword.
  }
}

\section{Changes in version 1.0.1}{
  \itemize{
    \item \code{pgmm_marginal} and other functions in \code{src/GMCM.cpp} have
      been rewritten to address memory issues. \cpkg{GMCM} should now be more
      stable and faster.
  }
}



