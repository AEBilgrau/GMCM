context("Check classify")

d <- 2
m <- 3
n <- 50
theta <- rtheta(m = m, d = d, method = "EqualEllipsoidal")
x <- matrix(runif(n*m), n, m)

test_that("classify returns expected types", {
  out <- classify(x = x, theta = theta)
  expect_true(is.numeric(out))
  expect_length(out, n)
  expect_lte(max(out), 3)
  expect_gte(min(out), 1)
})
