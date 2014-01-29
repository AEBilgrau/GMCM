\name{is.theta}
\alias{is.theta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check if parameters are valid
}
\description{
Function to check whether the argument is coherent and in the correct format.
}
\usage{
is.theta(theta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
A list on the \code{theta}-form described in \code{\link{rtheta}}
}
}
\value{
\code{logical}. Return \code{TRUE} if \code{theta} is coherent and in the correct format. Otherwise, returns \code{FALSE} with warning message.
}

\author{
Anders Ellern Bilgrau (abilgrau@math.aau.dk)
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{rtheta}}
}
\examples{
  theta1 <- rtheta()
  GMCM:::is.theta(theta1)
  
  theta2 <- rtheta(d=3,m=5)
  theta2$m <- 6  # m is now incoherent with the number of components
  GMCM:::is.theta(theta2)
  
  theta3 <- rtheta(d=4,m=2)
  theta3$sigma$comp1[1,2] <- 0 
  GMCM:::is.theta(theta3)
  
  theta4 <- rtheta(d=10,m=10)
  theta4$sigma$comp1[1,1] <- 0 
  GMCM:::is.theta(theta4)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
