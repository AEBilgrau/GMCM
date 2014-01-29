is.theta <- function(theta) {
  # Testing structure of theta
  if (!is.list(theta) | length(theta) != 5) {
    warning("theta is not a list of length 5")
    return(FALSE)
  }
  if (!is.list(theta[[4]])) {
    warning("theta[[4]] is not a list")
    return(FALSE)
  }
  if (!is.list(theta[[5]])) {
    warning("theta[[5]] is not a list")
    return(FALSE)
  }
  for (i in 1:2) {
    if (!is.numeric(theta[[i]]) | !length(theta[[i]]) == 1) {
      warning("theta[[",i,"]] is not a numeric vector of length 1")
      return(FALSE)
    }
  }
  # Testing mixture proportions
  if (length(theta[[3]]) != theta[[1]]) {
    warning("theta[[3]] is not a vector of length ", theta[[1]], " as defined",
            " by theta[[1]]")
    return(FALSE)
  }
  if (1 - sum(theta[[3]]) > .Machine$double.eps) {
    warning("The mixture proportions theta[[3]] does not sum to 1!")
    return(FALSE) 
  }
  # Testing mean vectors 
  if (!all(sapply(theta[[4]], length) ==  theta[[2]])) {
    warning("The length of the vectors in theta[[4]] does not equal ",
            theta[[2]], " as defined in theta[[2]]")
    return(FALSE)
  }
  # Testing covariance matrices
  if (length(theta[[5]]) != theta[[1]]) {
    warning("theta[[5]] is not a list of length ", theta[[1]], " as given by",
            " theta[[1]]")
    return(FALSE)
  }
  if (!all(c(sapply(theta[[5]], dim)) ==  theta[[2]])) {
    warning("The covariance matrices in theta[[5]] does not have dimensions ",
            theta[[2]], "x", theta[[2]], " as given by theta[[2]]")
    return(FALSE)
  }
  is.PosDef <- function(x) {
    all(eigen(x)$values >= 0)
  }
  for (i in 1:theta[[1]]) {
    if (!isSymmetric(theta[[5]][[i]])) {
      warning("Not all covariance matrices are symmetric")
      return(FALSE)
    }
    if (!is.PosDef(theta[[5]][[i]])) {
      warning("Not all covariance matrices are postive semi definite")
      return(FALSE)
    }
  }
  return(TRUE)
}
