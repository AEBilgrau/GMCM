#' Coerce a list to a theta object
#'
#' A function that attempts to coerce a theta-like list into a proper formatted
#' object of class \code{theta}.
#'
#' @details
#' First, if the list is of length 3 and not 5, the number of components and
#' dimension is assumed to be missing and added.
#' Secondly, the class is added.
#' Thirdly, names are added if needed.
#' Next, matrix means and array covariances are
#' coerced to list form.
#' Covariances on array form are assumed to be \code{d} by \code{d} by \code{m}.
#' Means on matrix form are as assumed to be \code{d} by \code{m}. I.e.
#' rows correspond to the dimensions and columns to components, or the mean vectors
#' as column vectors.
#' Finally, the sum constraint of 1 for the mixture proportions is enforced.
#'
#' @param x A theta-like object that can be coerced.
#' @return A theta object. See \code{\link{rtheta}}.
#' @examples
#' m <- 2
#' d <- 3
#' x <- list(m = m,
#'           d = d,
#'           pie = c(0.5, 0.5),
#'           mu = list(comp1=rep(0,d), comp2=rep(1,d)),
#'           sigma = list(comp1=diag(d), comp2=diag(d)))
#' print(x)
#' theta <- as.theta(x)
#' print(theta)
#'
#' x2 <- unname(list( # Unnamed
#'   # missing m and d
#'   pie = c(1, 1),   # Does not sum to 1
#'   mu = simplify2array(list(comp1=rep(0,d), comp2=rep(1,d))), # matrix, not a list
#'   sigma = simplify2array(list(comp1=diag(d), comp2=diag(d)))  # array, not a list
#' ))
#' theta2 <- as.theta(x2)
#' print(theta2)
#' @export
as.theta <- function(x) {
  # Discard if any entries are null
  x <- x[!sapply(x, is.null)]

  # Reconstruct length to 5
  if (length(x) == 3) {
      m <- length(x[[1]]) # x[[1]] assumed to be "pie"

      if (is.matrix(x[[2]]) && is.numeric(x[[2]])) { # x[[2]] assumed to be "mu"
        d <- nrow(x[[2]])
      }
      if (is.list(x[[2]])) {
        d <- length(x[[2]][[1]])
      }
      x <- c(list(m = m, d = d), x)
  }

  # Add class
  class(x) <- "theta"

  # Attempt to name components
  if (is.null(names(x)) || any(names(x) == "")) {
    the_names <- c("m", "d", "pie", "mu", "sigma")
    if (!is.null(names(x))) {
      if (!all(names(x)[names(x) != ""] == the_names[names(x) != ""])) {
        stop("The partial named list x do not match names to be applied.")
      }
    }
    names(x) <- the_names
  }

  # Convert 'matrix' means to list
  if (is.matrix(x[[4]]) && is.numeric(x[[4]])) {
    stopifnot(nrow(x[[4]]) == x[[2]], # d
              ncol(x[[4]]) == x[[1]]) # m
    x[[4]] <- structure(lapply(seq_len(x[[1]]), function(j) x[[4]][, j]),
                        names = colnames(x[[4]]))
  }

  # Convert higher order array covariances to list
  if (is.array(x[[5]]) && is.numeric(x[[5]])) {
    stopifnot(dim(x[[5]]) == c(x[[2]], x[[2]], x[[1]]))
    x[[5]] <- structure(lapply(seq_len(x[[1]]), function(k) x[[5]][, , k]),
                        names = dimnames(x[[5]])[[3]])
  }

  # Enforce sum contraint of pie to 1
  if (!isTRUE(all.equal(sum(x$pie), 1))) {
    x$pie <- x$pie/sum(x$pie)
    warning("x$pie rescaled to enforce sum constraint of 1")
  }

  # Handle sigma names
  for (k in seq_len(x$m)) {
    sigma_k <- x$sigma[[k]]

    # Rownames present colnames missing
    if (is.null(rownames(sigma_k)) && !is.null(colnames(sigma_k))) {
      rownames(sigma_k) <- colnames(sigma_k)
    }

    # Colnames present rownames missing
    if (!is.null(rownames(sigma_k)) && is.null(colnames(sigma_k))) {
      colnames(sigma_k) <- rownames(sigma_k)
    }

    # If both are present, but unequal
    if (!is.null(rownames(sigma_k)) && !is.null(colnames(sigma_k)) &&
        !identical(rownames(sigma_k), colnames(sigma_k))) {
      rownames(sigma_k) <- colnames(sigma_k)
    }

    x$sigma[[k]] <- sigma_k
  }

  # Test if coercion is complete
  if (is.theta(x)) {
    return(x)
  } else {
    stop("Could not coerce 'x' into a theta object.")
  }
}
