choose.theta <- function(u, m, fac = 2, ...) {

  km <- kmeans(u, centers = m, ...)
  pie <- km$size/sum(km$size)
  mu <- lapply(1:m, function(i) km$centers[i, ])

  get.sigma <- function(i) {
    diag(colSds(u[km$cluster == i, ]))/sqrt(km$size[i])
  }
  sigma <- lapply(1:m, get.sigma)
  sigma <- lapply(sigma, "*", fac^2)
    
  # Scaling and translating
  mu      <- lapply(mu, "-", mu[[1]])           # Translating means
  
  scaling <- diag(sigma[[1]])[1]                # Get scaling factor
  sigma   <- lapply(sigma, "/", scaling)        # Scaling sigma
  mu  <- lapply(mu, "/", sqrt(scaling)) # Scaling means
  
  # Naming
  names(pie) <- paste("pie", 1:m, sep = "")
  names(mu) <- names(sigma) <- paste("comp", 1:m, sep = "")
  ans <- list(m = m, d = ncol(u), pie = pie, mu = mu, sigma = sigma)

  return(ans)
}
