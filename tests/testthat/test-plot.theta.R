context("Check plot.theta")


theta <- rtheta(d = 3, m = 2)  # Create a random correctly formatted theta


test_that("plotting theta objects works", {
  skip_on_cran()
  skip_on_travis()

  file1 <- tempfile(fileext = ".pdf")
  pdf(file1)
  out <- plot(theta, add.ellipses = TRUE)
  dev.off()
  unlink(file1)

  expect_true(is.list(out))
  expect_identical(length(out), 3L)
  expect_identical(names(out), letters[24 + 0:2])
})


test_that("plotting theta objects with non-default arguments", {
  skip_on_cran()
  skip_on_travis()

  file2 <- tempfile(fileext = ".pdf")
  pdf(file2)
  out <- plot(theta,
              which.dims = 3:2,
              n.sd = 1.96,
              add.means = TRUE,
              nlevels = 40,
              axes = FALSE,
              xlab = "my dim 3",
              ylab = "my dim 2",
              add.ellipses = TRUE)
  dev.off()
  unlink(file2)


  expect_true(is.list(out))
  expect_identical(length(out), 3L)
  expect_identical(names(out), letters[24 + 0:2])
})



