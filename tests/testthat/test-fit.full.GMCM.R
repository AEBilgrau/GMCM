context("Check fit.full.GMCM")


sim <- SimulateGMCMData(n = 1000, m = 3, d = 2)


test_that("fit.full.GMCM run and returns an is.theta object", {

  # Test 1
  res1 <- fit.full.GMCM(u = sim$u, m = 3,
                       method = "NM", max.ite = 3000,
                       reltol = 1e-2, trace = FALSE)

  expect_true(is.theta(res1))

  # Test 2
  expect_output({
    res2 <- fit.full.GMCM(u = sim$u, m = 3,
                          method = "PEM", max.ite = 3000,
                          trace = TRUE)
  })
  expect_true(is.theta(res2))


})
