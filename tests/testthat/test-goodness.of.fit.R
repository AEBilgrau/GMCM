context("Check goodness.of.fit function")

theta <- rtheta()
u <- Uhat(SimulateGMCMData(theta = theta)$u)

test_that("goodness.of.fit is working as intended", {

  goodness.of.fit(theta, u) %>%
    expect_length(1) %>%
    expect_type("double")

  goodness.of.fit(theta, u, method = "AIC", k = 3) %>%
    expect_length(1) %>%
    expect_type("double")

  goodness.of.fit(theta, u, method = "BIC") %>%
    expect_length(1) %>%
    expect_type("double")
})

