Uhat <- function (x) {  # Ranking function
  if (is.vector(x)) {
    x <- matrix(x, length(x), 1)
  }
  base::apply(x, 2, rank, ties.method = "max")/(base::nrow(x) + 1)
}
