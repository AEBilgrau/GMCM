#' Fast ranking function
#' 
#' Function for computing the scaled ranks for each column of the input matrix.
#' I.e. the values are ranked column-wise and divided by \code{nrow(x) + 1}.
#' 
#' @param x A matrix of observations to be ranked. Rows correspond to features
#'   and columns to experiments.
#' @return A matrix with the same dimensions as \code{x} of the scaled ranks.
#' @author Anders Ellern Bilgrau (abilgrau@@math.aau.dk)
#' @seealso \code{\link{SimulateGMMData}}, \code{\link{SimulateGMCMData}}
#' @examples
#' data <- SimulateGMMData()
#' par(mfrow = c(1,2))
#' plot(data$z, xlab = expression(z[1]), ylab = expression(z[2]))
#' plot(Uhat(data$z), xlab = expression(hat(u)[1]), ylab = expression(hat(u)[2]))
Uhat <- function (x) {  # Ranking function
  if (is.vector(x)) {
    x <- matrix(x, length(x), 1)
  }
  apply(x, 2, rank, ties.method = "max")/(nrow(x) + 1)
}
