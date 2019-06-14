context("Check is.theta and rtheta")

theta1 <- rtheta()  # Create a random correctly formatted theta

theta2 <- rtheta(d = 3, m = 5)
theta2$m <- 6  # m is now incoherent with the number of components

theta3 <- rtheta(d = 4, m = 2)
theta3$sigma$comp1[1, 2] <- 0  # Making the covariance matrix non-symmetric

theta4 <- rtheta(d = 10, m = 10)
theta4$sigma$comp1[1, 1] <- 0  # Destroy positive semi-definiteness

theta5 <- rtheta(d = 2, m = 5)
names(theta5)[3] <- "prop" # Destory naming convention

theta6 <- rtheta(d = 2, m = 3)
theta6[[4]] <- simplify2array(theta6[[4]]) # Improper mu element

theta7 <- rtheta(d = 3, m = 2)
theta7[[5]] <- simplify2array(theta7[[5]]) # Improper sigma element

theta8 <- rtheta(d = 3, m = 2)
theta8[[1]] <- c(1,1) # Improper m

theta9 <- rtheta(d = 4, m = 3)
theta9[[2]] <- c(2,2,2) # Improper d

theta10 <- rtheta(d = 4, m = 3)
theta10[[3]][1] <- theta10[[3]][1] + 0.1 # Improper mix props

theta11 <- rtheta(d = 4, m = 3)
theta11[[5]] <- theta11[[5]][-1] # Improper number of sigmas

theta12 <- rtheta(d = 4, m = 3)
theta12[[5]] <- lapply(theta12[[5]], function(x) x[-1,-1]) # Improper dimension of sigmas

# Construct theta with sigma rownames only, colnames only, both, and no dimnames
theta13 <- rtheta(d = 5, m = 4)
dnames <- paste("dim", 1:5)
rownames(theta13$sigma$comp1) <- dnames
colnames(theta13$sigma$comp2) <- dnames
rownames(theta13$sigma$comp3) <- colnames(theta13$sigma$comp3)  <- dnames

test_that("rtheta returns proper formatted output (old method)", {
  expect_true(is.logical(is.theta(theta1)))
  expect_that(length(is.theta(theta1)), equals(1))
  expect_true(is.theta(theta1))

  suppressWarnings({
    expect_false(is.theta(theta2))
    expect_false(is.theta(theta3))
    expect_false(is.theta(theta4))
    expect_false(is.theta(theta5))
    expect_false(is.theta(theta6))
    expect_false(is.theta(theta7))
    expect_false(is.theta(theta8))
    expect_false(is.theta(theta9))
    expect_false(is.theta(theta10))
    expect_false(is.theta(theta11))
    expect_false(is.theta(theta12))

    expect_true(is.theta(theta13))
  })
})

#
# Test all rtheta methods and many d and m
#

set.seed(10)

for (d in 2:10) {
  for (m in 2:10) {
    for (method in as.character(formals(rtheta)$method)[-1]) {
      theta.tmp <- rtheta(m = m, d = d, method = method)
      test_that(paste("rtheta returns proper formatted output, ",
                      "m=", m, ", d=", d, ", method=", method, sep = ""), {
         expect_true(is.theta(theta.tmp))
      })
    }
  }
}

#
# Class checks
#

class(theta1) <- NULL
test_that('Test is.theta check.class arugment', {
  expect_false(suppressWarnings(is.theta(theta1)))
  expect_true( suppressWarnings(is.theta(theta1, check.class = FALSE)))
})

# Test degenerate input
