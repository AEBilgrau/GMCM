\name{cummean}
\alias{cummean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Cumulative mean values
}
\description{
 Returns a vector whose \code{i}'th element is the cumulative mean (arithmetic mean) of the \code{i}'th first elements of the argument.
}
\usage{
cummean(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
 a numeric vector.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A vector of length \code{length(x)} with the cumulative mean. The \code{i}'th entry \code{cummean(x)[i]} equals \code{mean(x[1:i])}.
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
\code{\link{cumsum}}
}
\examples{
  x <- sort(rnorm(100))
  GMCM:::cummean(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
