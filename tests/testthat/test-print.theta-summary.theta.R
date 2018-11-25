context("Check print.theta and summary.theta")

theta <- rtheta()  # Create a random correctly formatted theta

test_that("Printing theta works", capture_output({
  expect_output(print(theta))
  expect_output(summary(theta))
  expect_identical(theta, print(theta))
  expect_identical(theta, summary(theta))
}))
