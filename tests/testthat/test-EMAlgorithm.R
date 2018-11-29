context("Check EMAlgorithm")

set.seed(1)
n <- 1000

test_that("EMAlgorithm returns properly formatted output", {
  for (d in 3:4) {
    for (m in 3) {
      data <- SimulateGMMData(n = n, d = d, m = m)  # Simulate some data
      start.theta <- rtheta(d = d, m = m)

      for (trace_theta in c(TRUE, FALSE)) {
        res <- GMCM:::EMAlgorithm(data$z, theta = start.theta, max.ite = 100,
                                  trace.theta = trace_theta)

        expect_that(length(res), equals(ifelse(trace_theta, 4L, 3L)))
        expect_that(is.theta(res$theta), is_true())  # theta
        expect_that(is.numeric(res$loglik.tr), is_true())
        expect_that(length(res$loglik.tr), is_less_than(101))
        expect_that(is.matrix(res$kappa), is_true())
        expect_that(max(res$kappa), is_less_than(1 + 1e-10))
        expect_that(min(res$kappa), is_more_than(0 - 1e-10))
        expect_that(dim(res$kappa), equals(c(n, start.theta$m)))
        expect_that(length(res$theta$mu), equals(m))
        expect_that(length(res$theta$sigma), equals(m))
        expect_that(all(sapply(res$theta.tr, is.theta)), is_true())

      }
    }
  }
})

# Test failures

m <- 2
d <- 3

test_that("EMAlgorithm warns if max iterations is hit", {
  data <- SimulateGMMData(n = n, d = d, m = m)  # Simulate some data
  expect_warning(
    GMCM:::EMAlgorithm(data$z, theta = rtheta(d = d, m = m), max.ite = 5)
  )
})

test_that("EMAlgorithm is verbose if asked", {
  data <- SimulateGMMData(n = n, d = d, m = m)  # Simulate some data
  expect_output(
    GMCM:::EMAlgorithm(data$z, theta = rtheta(d = d, m = m), verbose = TRUE)
  )
})

