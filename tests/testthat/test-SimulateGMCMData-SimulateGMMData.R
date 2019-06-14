context("Check SimulateGMCMData and SimualteGMMData")

for (d in 2:4) {
  for (n in c(10, 100, 1000)) {
    suppressWarnings({
      gmm <- SimulateGMMData(n = n, theta = rtheta(d = d))
    })

    test_that("SimulateGMMData returns proper format", {
      expect_true(is.list(gmm))
      expect_that(length(gmm), equals(3))
      expect_that(names(gmm), equals(c("z", "K", "theta")))
      # Check z
      expect_true(is.matrix(gmm$z))
      expect_true(is.numeric(gmm$z))
      expect_that(dim(gmm$z), equals(c(n, d)))
      # Check K
      expect_true(is.numeric(gmm$K))
      expect_that(length(gmm$K), equals(n))
      # Check theta
      expect_true(is.theta(gmm$theta))
    })

    suppressWarnings({
      gmcm <- SimulateGMCMData(n = n, theta = rtheta(d = d))
    })

    test_that("SimulateGMCMData returns proper format", {
      expect_true(is.list(gmcm))
      expect_that(length(gmcm), equals(4))
      expect_that(names(gmcm), equals(c("u", "z", "K", "theta")))
      # Check u
      expect_true(is.matrix(gmcm$u))
      expect_true(is.numeric(gmcm$u))
      expect_that(dim(gmcm$u), equals(c(n, d)))
      # Check z
      expect_true(is.matrix(gmcm$z))
      expect_true(is.numeric(gmcm$z))
      expect_that(dim(gmcm$z), equals(c(n, d)))
      # Check K
      expect_true(is.numeric(gmcm$K))
      expect_that(length(gmcm$K), equals(n))
      # Check theta
      expect_true(is.theta(gmcm$theta))
    })

  }
}
