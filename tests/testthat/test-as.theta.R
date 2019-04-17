context("Check as.theta")

m <- 2
d <- 2
x <- list(m = m,
          d = d,
          pie = c(0.5, 0.5),
          mu = list(comp1 = rep(0,d), comp2 = rep(1,d)),
          sigma = list(comp1 = diag(d), comp2 = diag(d)))

test_that("Check that as.theta works as intended", {
  expect_true(is.theta(as.theta(x)))
  suppressWarnings(expect_error(as.theta(x[-4])))
})

# Coercion tests
# Various issues at once:
d <- 3
x2 <- list( # Unnamed list
  # Missing m and d entries
  c(1, 1),   # Does not sum to 1
  simplify2array(list(comp1 = rep(0,d), comp2 = rep(1,d))), # matrix, not a list
  simplify2array(list(comp1 = diag(d), comp2 = diag(d)))  # array, not a list
)

test_that("as.theta works for 'matrix means', 'array sigma',
          mixture proportions that do not sum to 1, and missing m and d", {
  expect_warning(
    expect_true(is.theta(as.theta(x2))),
    'rescaled'
  )
})


# Entries m and d as null (but not implicitly missing)
x3 <- x
x3["m"] <- list(NULL)
x3["d"] <- list(NULL)
test_that("as.theta works if m and d are NULL", {
  expect_true(is.theta(as.theta(x3)))
})


# Covariance rownames missing wile colnames are not
d <- 2
x4 <- list(pie = c(1, 1, 1)/3,
           mu = list(comp1 = rep(0,d), comp2 = rep(1,d), comp3 = rep(1,d)),
           sigma = list(comp1 = diag(d), comp2 = diag(d), comp3 = diag(d)))
colnames(x4$sigma$comp1) <- c("dim1", "dim2")
rownames(x4$sigma$comp2) <- c("dim1", "dim2")
dimnames(x4$sigma$comp3) <- list(c("dim1", "dim2"), c("d1", "d2"))

test_that("as.theta works with missing covaraince colnames/rownames (#33)", {
  expect_silent(as.theta(x4))
  expect_s3_class(as.theta(x4), "theta")
  expect_type(as.theta(x4), "list")
  expect_length(as.theta(x4), 5)
  expect_true(is.theta(as.theta(x4)))
  expect_named(as.theta(x4))
})



