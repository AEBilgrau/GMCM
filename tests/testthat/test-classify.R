context("Check classify")

# Create some test data
d <- 2
m <- 3
n <- 50

test_that("classify returns expected types and ranges using theta", {
  theta <- rtheta(m = m, d = d, method = "EqualEllipsoidal")
  u <- SimulateGMCMData(n, theta = theta)$u
  out <- classify(x = u, theta = theta)
  expect_true(is.numeric(out))
  expect_length(out, n)
  expect_lte(max(out), m)
  expect_gte(min(out), 1)
})

test_that("classify returns expected types and ranges using probabilities", {
  prob <- matrix(runif(n*m), n, m)
  out <- classify(x = prob)
  expect_true(is.numeric(out))
  expect_length(out, n)
  expect_lte(max(out), m)
  expect_gte(min(out), 1)
})

# Test edge cases

# m = 1 class, d = 2
d <- 2
m <- 1
n <- 25
theta <- rtheta(m = m, d = d, method = "EqualEllipsoidal")
u <- SimulateGMCMData(n, theta = theta)$u

test_that("classify returns only one class", {
  out <- classify(x = u, theta = theta)
  expect_true(is.numeric(out))
  expect_length(out, n)
  expect_lte(max(out), m)
  expect_gte(min(out), 1)
})

