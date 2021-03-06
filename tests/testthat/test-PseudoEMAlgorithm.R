context("Check PseudoEMAlgorithm")

set.seed(319561)
d <- 2
n <- 1000
m <- 2
true.par <- c(runif(1, 0.2, 0.8), rnorm(1, 3, 0.5),
              rchisq(1, df = 1), runif(1, 0.5, 0.95))
data <- SimulateGMCMData(n = n, par = true.par, d = d)
uhat <- Uhat(data$u)  # Observed ranks

# Fit the model using the Pseudo EM algorithm
init.par <- c(0.5, 1, 1, 0.5)
suppressWarnings({
  res <- GMCM:::PseudoEMAlgorithm(uhat, meta2full(init.par, d = d),
                                  verbose = FALSE, eps = 1e-3,
                                  trace.theta = FALSE,
                                  convergence.criterion = "absGMCM",
                                  meta.special.case = TRUE)
})

test_that("PseudoEMAlgorithm returns proper format", {
  expect_true(is.list(res))
  expect_that(length(res), equals(3))
  expect_true(is.theta(res$theta))

  expect_true(is.matrix(res$loglik.tr))
  expect_that(nrow(res$loglik.tr), equals(3))

  expect_true(is.matrix(res$kappa))
  expect_that(dim(res$kappa), equals(c(n, m)))
})


test_that("PseudoEMAlgorithm expected errors & warnigns", {
  expect_error(GMCM:::PseudoEMAlgorithm(uhat, meta2full(c(0.5, 25, 0.1, 0.8), d = d)))
  expect_warning(GMCM:::PseudoEMAlgorithm(uhat, meta2full(c(0.5, 2,  0.1, 0.8), d = d),
                                          max.ite = 2))
})

