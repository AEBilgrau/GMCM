context("Check get.idr, get.IDR, get.prob")


#' Tests if value within a range
#'
#' This function tests if the range of the returned values is within
#' the interval from \code{a} to \code{b}.
#'
#' @param object The object to be tested.
#' @param lower The lower limit.
#' @param upper The upper limit.
#' @param included Specifies which limits are included in the interval.
#' @author
#'   Anders Ellern Bilgrau <anders.ellern.bilgrau (at) gmail.com> \cr
#' @seealso \code{\link{is_less_than}} \code{\link{is_more_than}}
#' @examples
#' library("testthat")
#' expect_between(runif(1), 0, 1)
#' expect_between(rnorm(1), -2, 2)
#' expect_between(2, -2, 2, included = "upper")
#' expect_between(2, -2, 2, included = "none")
expect_between <- function(object, lower = 0, upper = 1,
                           included = c("both", "lower", "upper", "none")) {
  included <- match.arg(included)
  if (included %in% c("both", "upper")) {
    act1 <- expect_lte(max(object), upper)
  } else {
    act1 <- expect_lt(max(object), upper)
  }
  if (included %in% c("both", "upper")) {
    act2 <- expect_gte(min(object), lower)
  } else {
    act2 <- expect_gt(min(object), lower)
  }
  if (exists("act1") && exists("act2") && identical(act1, act2)) {
    return(invisible(act1))
  }
}

true.par <- c(0.9, 2, 0.7, 0.6)
n <- 1000
data <- SimulateGMCMData(n = n, par = true.par, d = 2)


# res1
test_that("test get.IDR result format", {

  res1_pri <- get.IDR(data$u, true.par, threshold = 0.5)
  res1_alt <- get.IDR(data$u, true.par, threshold = 0)

  for (res1 in list(res1_pri, res1_alt)) {

    expect_that(length(res1), equals(5))
    expect_true(is.list(res1))
    expect_that(names(res1), equals(c("idr", "IDR", "l", "threshold", "Khat")))

    # idr
    expect_true(is.numeric(res1$idr))
    expect_that(length(res1$idr), equals(n))
    expect_between(res1$idr, 0, 1)

    # IDR
    expect_true(is.numeric(res1$IDR))
    expect_that(length(res1$IDR), equals(n))
    expect_between(res1$IDR, 0, 1)

    # l
    expect_true(is.numeric(res1$l))
    expect_that(length(res1$l), equals(1))
    expect_that(res1$l, equals(sum(res1$IDR < res1$threshold)))
    expect_between(res1$l, 0, n)

    # Khat
    expect_true(is.numeric(res1$Khat))
    expect_that(length(res1$Khat), equals(n))
    expect_between(res1$Khat, 1, data$theta$m)
  }
})


# res2
res2 <- GMCM:::get.idr(data$u, data$theta)
test_that("test get.idr result format", {
  res2 <- GMCM:::get.idr(data$u, data$theta)
  expect_true(is.numeric(res2))
  expect_that(length(res2), equals(n))
  expect_between(res2, 0, 1)
})


# res3


test_that("test get.prob result format", {
  res3 <- get.prob(data$u, data$theta)
  expect_true(is.numeric(res3))
  expect_true(is.matrix(res3))
  expect_that(dim(res3), equals(c(n, data$theta$m)))
  expect_between(res3, 0, 1)
})


