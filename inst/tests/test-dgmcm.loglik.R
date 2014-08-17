context("Check dgmcm.loglik")


# The for proper return of size
for (n in c(5, 1232, 2313)) {
  for (m in c(2, 4, 5, 6))
  n <- 1000
  u <- SimulateGMCMData(n = n, m = m)$u
  theta <- rtheta(m = m)

  #dgmcm.loglik(theta, u, marginal.loglik = FALSE, ...)

  ans1 <- GMCM:::dgmcm.loglik(theta = theta, u = u)
  ans2 <- GMCM:::dgmcm.loglik(theta = theta, u = u, marginal.loglik = TRUE)

  test_that(paste0("dgmcm.loglik returns proper size (n = ",
                   n, ", m = ", m, ")"), {
    expect_that(ans1,         is_a("numeric"))
    expect_that(length(ans1), equals(1L))
    expect_that(ans2,         is_a("matrix"))
    expect_that(length(ans2), equals(n))
    expect_that(dim(ans2), equals(c(n,1)))
  })
}


# Test degenerated inputs



