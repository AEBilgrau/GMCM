context("Check as.theta")

m <- 2
d <- 2
x <- list(m = m,
          d = d,
          pie = c(0.5, 0.5),
          mu = list(comp1 = rep(0,d), comp2 = rep(1,d)),
          sigma = list(comp1 = diag(d), comp2 = diag(d)))

test_that("Check that as.theta works as intended", {
  expect_true(is.theta(as.theta(x)))
  suppressWarnings(expect_error(as.theta(x[-4])))
})

# Coercion tests
d <- 3
x2 <- list(m = m,
           d = d,
           pie = c(0.5, 0.5),
           mu = simplify2array(list(comp1 = rep(0,d), comp2 = rep(1,d))),
           sigma = simplify2array(list(comp1 = diag(d), comp2 = diag(d))))

test_that("Check that as.theta works for 'matrix means'", {
  expect_true(is.theta(as.theta(x2)))
})
