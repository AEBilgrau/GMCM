context("Check get.idr, get.IDR, get.prob")


#' Tests if the returned value(s) within a range
#'
#' This function tests if the range of the returned values is within
#' the interval from \code{a} to \code{b}.
#'
#' @param a The lower limit.
#' @param b The upper limit.
#' @author
#'   Anders Ellern Bilgrau <abilgrau(at)math.aau.dk> \cr
#'   (rewritten from  functions in the \testthat package by Hadley Wickham)
#' @seealso \code{\link{is_less_than}} \code{\link{is_more_than}}
#' @examples
#' library("testthat")
#' expect_that(runif(100), is_between(0, 1))
#' expect_that(rnorm(100), is_between(-2, 2))
is_between <- function(a, b, label = NULL, ...) {
  if (is.null(label)) {
    label <- find_expr("expected")
  }
  else if (!is.character(label) || length(label) != 1) {
    label <- deparse(label)
  }
  function(actual) {
    r <- range(actual)
    expectation(a <= r[1] && r[2] <= b,
                paste0("not between ", label, ". Range is from ",
                       format(r[1]), " to ", format(r[2]), "."),
                paste0("is between ", label))
  }
}
environment(is_between) <- asNamespace('testthat')



true.par <- c(0.9, 2, 0.7, 0.6)
n <- 1000
data <- SimulateGMCMData(n = n, par = true.par, d = 2)

l <- 0.5
res1 <- get.IDR(data$u, true.par, threshold = l)
res2 <- get.idr(data$u, data$theta)
res3 <- get.prob(data$u, data$theta)


test_that("test get.IDR result format", {
  # res1
  expect_that(length(res1), equals(5))
  expect_that(is.list(res1), is_true())
  expect_that(names(res1), equals(c("idr", "IDR", "l", "threshold", "Khat")))

  # idr
  expect_that(is.numeric(res1$idr), is_true())
  expect_that(length(res1$idr), equals(n))
  expect_that(res1$idr, is_between(0, 1))
  # IDR
  expect_that(is.numeric(res1$IDR), is_true())
  expect_that(length(res1$IDR), equals(n))
  expect_that(res1$IDR, is_between(0, 1))
  # l
  expect_that(is.numeric(res1$l), is_true())
  expect_that(length(res1$l), equals(1))
  expect_that(res1$l, equals(sum(res1$IDR < l)))
  expect_that(res1$l, is_between(0, n))
  # Khat
  expect_that(is.numeric(res1$Khat), is_true())
  expect_that(length(res1$Khat), equals(n))
  expect_that(res1$Khat, is_between(1, data$theta$m))
})


test_that("test get.idr result format", {
  # res2
  expect_that(is.numeric(res2), is_true())
  expect_that(length(res2), equals(n))
  expect_that(res2, is_between(0, 1))
})


test_that("test get.prob result format", {
  #res3
  expect_that(is.numeric(res3), is_true())
  expect_that(is.matrix(res3), is_true())
  expect_that(dim(res3), equals(c(n, data$theta$m)))
  expect_that(res3, is_between(0, 1))
})


