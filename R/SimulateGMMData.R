SimulateGMMData <- function(n = 1000, theta = rtheta(...), ...) {
  K <- sample(1:theta$m, size = n, replace = TRUE, prob = theta$pie)
  
  if (length(unique(K)) != theta$m) {
    warning(paste("Some components was not represented in sample.",
                  "The mixture proportions may be too small to be sampled.",
                  "Ignore this warning, try to resample, or change theta."))
  }
  
  tab <- table(K)

  SampFunc <- function (i) {
    comp.k <- as.numeric(names(tab[i]))
    mvtnorm::rmvnorm(tab[i], theta$mu[[comp.k]], theta$sigma[[comp.k]])
  }
  
  z <- do.call(rbind, lapply(1:length(tab), FUN = SampFunc))[order(order(K)), ]
  return(list(z = unname(z), K = K, theta = theta))
}