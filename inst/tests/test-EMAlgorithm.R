context("Check EMAlgorithm")

set.seed(1)
n <- 10000

d <- 3
m <- 3
data <- SimulateGMMData(n = n, d = d, m = m)  # Simulate some data
start.theta <- rtheta(d = d, m = m)

#km <- kmeans(data$z, centers = m)$centers
#start.theta$mu <- lapply(1:m, function(i) km[i, ])
#start.theta$sigma <- replicate(m, diag(d), simplify = FALSE)
#stopifnot(is.theta(start.theta))

res <- GMCM:::EMAlgorithm(data$z, theta = start.theta, max.ite = 100,
                          trace.theta = TRUE)

test_that(paste0("EMAlgorithm returns properly formatted output"), {
  expect_that(is.theta(res$theta), is_true())  # theta
  expect_that(is.numeric(res$loglik.tr), is_true())
  expect_that(length(res$loglik.tr), is_less_than(101))
  expect_that(is.matrix(res$kappa), is_true())
  expect_that(max(res$kappa), is_less_than(1 + 1e-10))
  expect_that(min(res$kappa), is_more_than(0 - 1e-10))
  expect_that(dim(res$kappa), equals(c(n, start.theta$m)))
})


# Test more parameters!
