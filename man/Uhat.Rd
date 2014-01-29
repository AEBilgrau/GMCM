\name{Uhat}
\alias{Uhat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Fast ranking function
}
\description{
Function for computing the scaled ranks for each column of the input matrix. I.e. the values are ranked column-wise and divided by \code{nrow(x) + 1}. 
}
\usage{
Uhat(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  A matrix of observations to be ranked. Rows correspond to features and columns to experiments.
}
}
%\details{
% some details?
%}
\value{
 A matrix with the same dimensions as \code{x} of the scaled ranks.
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
\code{\link{SimulateGMMData}}, \code{\link{SimulateGMCMData}}
}
\examples{
data <- SimulateGMMData()
par(mfrow = c(1,2))
plot(data$z, xlab = expression(z[1]), ylab = expression(z[2]))
plot(Uhat(data$z), xlab = expression(hat(u)[1]), ylab = expression(hat(u)[2]))
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
