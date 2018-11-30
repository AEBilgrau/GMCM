context("Check qgmm.marginal")
set.seed(543485621)

for (d in seq_len(3) + 1) {
  for (n in trunc(seq(50, 50000, length.out = 20))) {

    sim <- SimulateGMCMData(n = n, theta = rtheta(d = d))
    dimnames(sim$z) <- list(paste0("f", 1:n), paste0("d", 1:d))
    theta <- rtheta(d = d)

    ans <- GMCM:::qgmm.marginal(u = sim$u, theta = theta)

    test_that("qgmm.marginal returns proper format", {
      expect_true(is.numeric(ans))
      expect_equal(dim(ans), c(n, d))
      expect_false(any(is.na(ans)))
    })

    test_that("qgmm.marginal handling of extrapolation", {
      u <- sim$u
      u[1, ] <- rep(1e-50, theta$d)
      u[2, ] <- rep(1 - 1e-50, theta$d)
      ans <- GMCM:::qgmm.marginal(u = u, theta = theta, rule = 1)
      expect_true(is.numeric(ans))
      expect_equal(dim(ans), c(n, d))
      expect_false(any(is.na(ans)))
      expect_equal(ans[1, ], rep(-Inf, d))
      expect_equal(ans[2, ], rep(Inf, d))
    })


  }
}
