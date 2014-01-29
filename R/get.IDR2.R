get.IDR <- function (x, par, threshold = 0.05, ...) {
  theta <- meta2full(par, d = ncol(x))
  idr  <- get.idr(x, theta, ...)
  ord  <- order(idr)
  IDR  <- cummean(idr[ord])
  if (any(IDR < threshold)) {
    l <- max(which(IDR < threshold))
  } else {
    l <- 0
  }
  IDR  <- IDR[order(ord)]
  Khat <- c(rep(2, l), rep(1, length(idr) - l))[order(ord)]
  return(list(idr = idr, IDR = IDR, l = l, threshold = threshold, Khat = Khat))
}