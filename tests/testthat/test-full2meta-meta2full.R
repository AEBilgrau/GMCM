context("Check full2meta and meta2full")

m <- 2
for (d in 2:10) {
  theta <- rtheta(m = m, d = d)
  theta$sigma$comp2[1,2] <- 0.5
  neg.eps <- .Machine$double.neg.eps
  eps     <- .Machine$double.eps

  par <- full2meta(theta)

  test_that("full2meta returns proper formatted output", {
    expect_true(is.numeric(par))
    expect_that(names(par),  equals(c("pie1", "mu", "sigma", "rho")))
    expect_that(length(par), equals(4))
    expect_that(par["pie1"], is_more_than(-neg.eps))     # 0 <= pie <= 1
    expect_that(par["pie1"], is_less_than(1 + eps))
    expect_that(par["sigma"], is_more_than(-neg.eps))    # Sigma >= 0
    expect_that(par["rho"], is_more_than(-1 - neg.eps))  # -1 <= rho <= 1
    expect_that(par["rho"], is_less_than(1 + eps))
  })

  test_that("full2meta fails when intended", {
    theta_f <- rtheta(m = m, d = d)
    theta_f$sigma[[2]][1,2] <- theta_f$sigma[[2]][2,1] <-
      tcrossprod(sqrt(diag(theta_f$sigma[[2]])))[1,2]*-(1/(d-1))*1.05

    expect_error(full2meta(theta_f))
    expect_error(full2meta(rtheta(m = 3, d = d)))
  })

  theta_back <- meta2full(par, d = d)

  test_that("meta2full returns proper formatted output", {
    expect_true(is.theta(theta_back))
    expect_that(theta_back$pie["pie1"],  equals(par["pie1"]))
    expect_that(theta_back$mu$comp2,     is_equivalent_to(rep(par["mu"], d)))
    expect_that(theta_back$sigma$comp1,  equals(diag(d)))
    expect_that(diag(theta_back$sigma$comp2),
                is_equivalent_to(rep(par["sigma"]^2, d)))
  })

  test_that("meta2full fail as intended", {
    expect_error(meta2full(c(0.5, 1, 1, -0.2), d = 10))
    expect_error(meta2full(c(0.5, 1, 1, -0.51), d = 3))
  })


}

# Test more parameters!
