\name{NEWS}
\title{News for Package \pkg{GMCM}}
\newcommand{\cpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\section{Changes to version 1.1}{
  \itemize{
    \item The \code{EStep} has been implemented in 
      \code{Rcpp} and thus GMCM now features a faster pseudo EM algorithm.
    \item Functions \code{rmvnormal} and \code{dmvnormal} are now exported and
      visible to the user. These are faster versions of \code{rmvnorm} and 
      \code{dmvnorm} from \cpkg{mvtnorm}.
    \item \cpkg{GMCM} is no longer dependent on the \cpkg{mvtnorm} package. 
      \cpkg{GMCM} now features faster simulation of data.
    \item A NEWS.Rd file have been added!
  }
}

\section{Changes to version 1.0.1}{
  \itemize{
    \item \code{pgmm_marginal} and other functions in \code{src/GMCM.cpp} have 
      been rewritten to address memory issues. \cpkg{GMCM} should now be more 
      stable and faster.
  }
}



