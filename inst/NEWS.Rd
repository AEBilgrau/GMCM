\name{NEWS}
\title{News for Package \pkg{GMCM}}
\newcommand{\cpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\section{Changes in version 1.1}{
  \itemize{
    \item Functions \code{rmvnormal} and \code{dmvnormal} are now exported an
      visible to the user. These are faster versions of \code{rmvnorm} and  \code{dmvnorm} from \pkg{mvtnorm}.
    \item \cpkg{GMCM} is no longer dependent on \cpkg{mvtnorm}. \cpkg{GMCM} now features faster simulation of data.
    \item A NEWS.Rd file have been added!
  }
}

\section{Changes in version 1.0.1}{
  \itemize{
    \item \code{pgmm_marginal} and other functions in \code{src/GMCM.cpp} have been
    rewritten and \cpkg{GMCM} should now be more stable and faster.
  }
}



