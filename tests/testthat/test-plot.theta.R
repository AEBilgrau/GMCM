context("Check plot.theta")

theta <- rtheta(m = 2, d = 2)  # Create a random correctly formatted theta
file <- tempfile(fileext = ".pdf")

test_that("plotting theta objects works", {
  skip_on_cran()
  pdf(file)
  out <- plot(theta, add.ellipses = TRUE)
  dev.off()
  expect_true(is.list(out))
  expect_identical(length(out), 3L)
  expect_identical(names(out), letters[24+0:2])
})

unlink(file)
