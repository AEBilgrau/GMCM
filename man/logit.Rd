\name{logit}
\alias{logit}
\alias{inv.logit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Logit and inverse logit transforms
}
\description{
The logit transformation (i.e. the log of the odds) and its inverse.
}
\usage{
logit(p)
inv.logit(a)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
  A vector of probabilities.
}
  \item{a}{
  A vector of real values.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
\code{logit} returns a vector of the same length as \code{p} with the log odds of \code{p}.

\code{inv.logit} returns a vector of the same length as \code{a} of the inverse logit transformed values. This function is also known as the expit-function.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Anders Ellern Bilgrau (abilgrau@math.aau.dk)
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Used in \code{\link{tt}} and \code{\link{inv.tt}}.
}
\examples{
p <- runif(100)
print(a <- GMCM:::logit(p))
p - GMCM:::inv.logit(a)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
