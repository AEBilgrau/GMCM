context("Check fit.meta.GMCM")

real.par <- c(0.6, 1.3, 2, 0.3)
sim <- SimulateGMCMData(n = 500, par = real.par, d = 6)


test_that("fit.meta.GMCM run and returns an is.theta object", {

  # Test 1
  res1 <- fit.meta.GMCM(u = sim$u, init.par = c(0.5, 1, 1, 0.5),
                        method = "L-BFGS", max.ite = 3000,
                        verbose = FALSE)


  expect_length(res1, 4)
  expect_true(is.numeric(res1))
  expect_lte(res1[1], 1)
  expect_gte(res1[1], 0)
  expect_lte(res1[4], 1)
  expect_gte(res1[4], 0)
  expect_gte(res1[3], 0)

  # Test 2
  expect_output({
    res2 <- fit.meta.GMCM(u = sim$u, init.par = c(0.5, 1, 1, 0.5),
                          method = "PEM", max.ite = 3000,
                          verbose = TRUE)
  })

  expect_length(res2, 4)
  expect_true(is.numeric(res2))
  expect_lte(res2[1], 1)
  expect_gte(res2[1], 0)
  expect_lte(res2[4], 1)
  expect_gte(res2[4], 0)
  expect_gte(res2[3], 0)


  # Test 3
  expect_output({
    res3 <- fit.meta.GMCM(u = sim$u, init.par = c(0.2, 0.5, 0.5, 0.2),
                          method = "NM", max.ite = 3000,
                          verbose = TRUE, trace.theta = TRUE)
  })

  expect_length(res3, 2)
  expect_true(is.list(res3))
  expect_true(is.numeric(res3[[1]]))
  expect_true(is.list(res3[[2]]))

})
