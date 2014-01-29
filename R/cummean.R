cummean <- function(x) { # Cumulative mean function
  cumsum(x)/seq_along(x)
}