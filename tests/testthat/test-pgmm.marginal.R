context("Check pgmm.marginal")

for (d in seq_len(3) + 1) {
  for (n in trunc(seq(50, 5000, length.out = 20))) {

    test_that("pgmm.marginal returns proper format", {

      sim <- SimulateGMMData(n = n,
                             theta = rtheta(d = d, method = "UnequalSpherical"))
      dimnames(sim$z) <- list(paste0("f", 1:n), paste0("d", 1:d))

      ans <-
        GMCM:::pgmm.marginal(z = sim$z,
                             theta = rtheta(d = d, method = "EqualEllipsoidal"))

      expect_that(is.numeric(ans), is_true())
      expect_that(dim(ans), equals(c(n, d)))
      expect_gte(min(ans), 0)
      expect_lte(max(ans), 1)
    })

    test_that("pgmm.marginal fails when intended", {

      sim <- SimulateGMMData(n = n,
                             theta = rtheta(d = d, method = "EqualEllipsoidal"))
      suppressWarnings({
        expect_error(GMCM:::pgmm.marginal(z = sim$z, theta = c(0.5, 1, 1, 0.5)),
                     "theta is formatted incorrectly")
        expect_error(GMCM:::pgmm.marginal(z = sim$z[1, ], theta = rtheta(d = d)),
                     "not a matrix")
        expect_error(GMCM:::pgmm.marginal(z = sim$z, theta = rtheta(d = 1)),
                     "Number of columns")
      })
    })


  }
}
