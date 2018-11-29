context("Check plot.theta")

theta <- rtheta()  # Create a random correctly formatted theta
file <- tempfile(fileext = ".pdf")

test_that("plotting theta objects works", {
  pdf(file)
  out <- plot(theta)
  dev.off()
  expect_true(is.list(out))
  expect_identical(length(out), 3L)
  expect_identical(names(out), letters[24+0:2])
})

theta <- rtheta(d = 3)

test_that("plotting theta objects ellipses", {
  pdf(file)
  out <- plot(theta, add.ellipses = TRUE)
  dev.off()
  expect_true(is.list(out))
  expect_identical(length(out), 3L)
  expect_identical(names(out), letters[24+0:2])
})


unlink(file)
