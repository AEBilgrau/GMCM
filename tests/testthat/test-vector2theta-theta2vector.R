context("Sanity check of vector2theta and theta2vector functions")

d <- 5
m <- 4
xtheta <- rtheta(m = m, d = d)

ytheta <- GMCM:::vector2theta(GMCM:::theta2vector(xtheta), d = 5, m = 4)
ztheta <- GMCM:::vector2theta(GMCM:::theta2vector(ytheta), d = 5, m = 4)

test_that("Theta should not change under repeated interation", {
  expect_that(ytheta,  equals(ztheta))
})

test_that("First component criteria is satisfied", {
  expect_that(ytheta$mu$comp1,  equals(rep(0, d)))
  expect_that(diag(ytheta$sigma$comp1),  equals(rep(1, d)))
})

test_that("vector2theta returns true 'theta' objects",{
  expect_true(is.theta(ytheta) && is.theta(ztheta))
})

