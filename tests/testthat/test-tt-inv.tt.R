context("Check tt and inv.tt")


test_that("tt and inv.tt returns proper formatted output", {

  n <- 1000
  par <- c(pie1 = 0.3, mu = 1, sigma = 0.5, rho = 0.8)
  tpar <- GMCM:::inv.tt(par, d = 3, positive.rho = FALSE)
  par2 <- GMCM:::tt(tpar, d = 3, positive.rho = FALSE)

  expect_that(is.numeric(tpar),  is_true())
  expect_that(length(tpar), equals(length(par)))
  expect_that(is.numeric(par2),  is_true())
  expect_that(length(par2),  equals(length(par)))
  expect_that(par - par2,
    equals(structure(c(0,0,0,0), names = c("pie1","mu","sigma","rho"))))
})

test_that("inv.tt fails as itended", {

  expect_error(GMCM:::inv.tt(c(pie1 = 0.3, mu =  1, sigma = -0.5, rho =  0.8), d = 2, positive.rho = TRUE))
  expect_error(GMCM:::inv.tt(c(pie1 = 1.3, mu =  1, sigma =  0.5, rho =  0.8), d = 2, positive.rho = TRUE))
  expect_error(GMCM:::inv.tt(c(pie1 = 0.3, mu = -1, sigma =  0.5, rho =  0.8), d = 2, positive.rho = TRUE))
  expect_error(GMCM:::inv.tt(c(pie1 = 0.3, mu =  1, sigma =  0.5, rho =  1.8), d = 2, positive.rho = TRUE))
  expect_error(GMCM:::inv.tt(c(pie1 = 0.3, mu =  1, sigma =  0.5, rho = -1.6), d = 2, positive.rho = FALSE))

})


# Test degenerate input
