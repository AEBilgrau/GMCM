context("Check Uhat function")

n <- 100
d <- 4
z <- SimulateGMCMData(n = n, d = d)$z


test_that("Uhat is working as intended", {
  res <- Uhat(z)
  # Structure
  expect_true(is.numeric(res))
  expect_true(is.matrix(res))
  expect_equal(dim(res), c(n, d))

  expect_equal(Uhat(res), res) # Idempotent
  expect_equal(res, apply(z, 2, function(v) ecdf(v)(v))*n/(n + 1)) # defintion
})


test_that("Uhat is working for vectors", {
  res2 <- Uhat(z[,1])
  # Structure
  expect_true(is.numeric(res2))
  expect_true(is.matrix(res2))
  expect_equal(dim(res2), c(n, 1))

  expect_equal(Uhat(res2), res2) # Idempotent
})


