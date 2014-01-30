Uhat <- function (x) {  # Ranking function
  if (is.vector(x)) {
    x <- matrix(x, length(x), 1)
  }
  apply(x, 2, rank, ties.method = "max")/(nrow(x) + 1)
}
