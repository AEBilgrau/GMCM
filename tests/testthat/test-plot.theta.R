context("Check plot.theta")

theta <- rtheta(d = 3, m = 2)  # Create a random correctly formatted theta
file <- tempfile(fileext = ".pdf")
pdf(file)

test_that("plotting theta objects works", {
  out <- plot(theta)

  expect_true(is.list(out))
  expect_identical(length(out), 3L)
  expect_identical(names(out), letters[24 + 0:2])
})


test_that("plotting theta objects with non-default arguments", {
  out <- plot(theta,
              which.dims = 3:2,
              n.sd = 1.96,
              add.means = TRUE,
              nlevels = 40,
              axes = FALSE,
              xlab = "my dim 3",
              ylab = "my dim 2",
              add.ellipses = TRUE)

  expect_true(is.list(out))
  expect_identical(length(out), 3L)
  expect_identical(names(out), letters[24 + 0:2])
})

dev.off()
unlink(file)

