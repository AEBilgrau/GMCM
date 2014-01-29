\name{rho.transform}
\alias{rho.transform}
\alias{inv.rho.transform}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transformation of the correlation to real line and its inverse
}
\description{
A transformation of the correlation coefficient into the real line and the corresponding inverse. The transform is a translation and scaling of \eqn{\rho}{rho} from the interval \eqn{(-1/(d-1), 1)}{(-1/(d-1), 1)} to \eqn{(0, 1)}{(0, 1)} followed by a logit transformation to the whole real line.
}
\usage{
rho.transform(rho, d)
inv.rho.transform(a, d)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rho}{
    A correlation coefficient between \code{-1/(d-1)} and \code{1}.
}
  \item{d}{
    The dimension of the space.
}
  \item{a}{
    A real number.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A vector of the transformed values with the same length as \code{rho} or \code{a}.
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
d <- 4
rho <- runif(100, -1/(d-1), 1)
a <- GMCM:::rho.transform(rho, d)
rho - GMCM:::inv.rho.transform(a, d)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
