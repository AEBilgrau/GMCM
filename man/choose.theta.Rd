\name{choose.theta}
\alias{choose.theta}

\title{
Heuristically chosen starting value of theta
}
\description{
This function uses a \code{k}-means algorithm to heuristically select suitable starting values for the general model.
}
\usage{
choose.theta(u, m, fac = 2, ...) 
}
\arguments{
  \item{u}{
  A matrix of (estimates of) realizations from the GMCM.
}
 \item{m}{
  The number of components to be fitted.
}
 \item{fac}{
  Numeric. A factor applied to the standard deviation estimates of each component and each dimension.
}
 \item{\dots}{
  Arguments passed to \code{\link{kmeans}}.
}
}
\details{
The function selects the centers from the k-means algorithm as an initial estimate of the means. The proportional sizes of the clusters are selected as the initial values of the mixture proportions. The within cluster standard deviations are used as the variance of the clusters. The correlations between each dimension are taken to be zero.
}
\value{
A list of parameters for the GMCM model on the form described in \code{\link{rtheta}}.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Anders Ellern Bilgrau (abilgrau@math.aau.dk)
}
\note{
The function uses the \code{kmeans} function from the \code{stats}-package.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
set.seed(2)

# Simulating data
data1 <- SimulateGMCMData(n = 10000, m = 3, d = 2)
obs.data <- Uhat(data1$u)  # The ranked observed data

# Using choose.theta to get starting estimates
theta <- choose.theta(u = obs.data, m = 3)
print(theta)

# To illustrate theta, we simulate from the model 
data2 <- SimulateGMMData(n = 10000, theta = theta)

cols <- apply(get.prob(obs.data,theta),1,which.max)

# Plotting
par(mfrow = c(1,3))
plot(data1$z, main = "True latent GMM")
plot(Uhat(data1$u), col = cols, 
     main = "Observed GMCM\nColoured by k-means clustering")
plot(data2$z, main = "initial GMM")
}
% Add one or more standard keywords, see file 'Kls()EYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
