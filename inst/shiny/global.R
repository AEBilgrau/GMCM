# Function for dput'ting objects into a character string
cput <- function(x) {
  f <- tempfile()
  dput(x, file = f)
  return(readLines(f))
}

# Function for plotting meta results
meta_plot <- function(fit, # A fitted object data
                      idr, # Output from get.IDR
                      plot_type = c("rank", "gmm", "obs"),
                      col_sel,
                      row_sel,
                      col = c(rep = "steelblue", irrep = "#00000050", sel = "red"),
                      pch = c(rep = 1, irrep = 1, sel = 16)) {

  if (is.null(col_sel) || length(col_sel) < 2) {
    i <- 1
    j <- 2
  } else {
    i <- match(col_sel[1], colnames(fit$u))
    j <- match(col_sel[2], colnames(fit$u))
  }

  # Get ranked data
  if (plot_type == "rank")  {
    x <- fit$u
  } else if (plot_type == "gmm") {
    x <- GMCM:::qgmm.marginal(fit$u, theta = meta2full(fit[[1]], d = ncol(fit$u)))
  } else if (plot_type == "obs") {
    x <- fit$x
  } else {
    stop("plot_type not found")
  }

  # Colour selected rows and reproducible
  n <- nrow(x)
  cols <- rep(col["irrep"], n)
  pchs <- rep(pch["irrep"], n)

  # Color by classification
  repro <- idr$Khat == 2
  cols[repro] <- col["rep"]
  pchs[repro] <- pch["rep"]
  cols[row_sel] <- col["sel"]
  pchs[row_sel] <- pch["sel"]

  rows_selected <- (seq_len(n) %in% row_sel)
  o <- order(repro, rows_selected)

  # Do plot
  plot(x = x[o, i],
       y = x[o, j],
       xlab = colnames(fit$u)[i],
       ylab = colnames(fit$u)[j],
       axes = FALSE,
       main = "",
       col = cols[o],
       pch = pchs[o],
       asp = 1)
  axis(1)
  axis(2)
}




# Function for plotting meta results
full_plot <- function(fit, # A fitted object data
                      comp, # Output from get.IDR
                      plot_type = c("rank", "gmm", "obs"),
                      col_sel,
                      row_sel,
                      pch = 16) {

  if (is.null(col_sel) || length(col_sel) < 2) {
    i <- 1
    j <- 2
  } else {
    i <- match(col_sel[1], colnames(fit$u))
    j <- match(col_sel[2], colnames(fit$u))
  }

  # Get ranked data
  if (plot_type == "rank")  {
    x <- fit$u
  } else if (plot_type == "gmm") {
    x <- GMCM:::qgmm.marginal(fit$u, theta = fit$theta)
  } else if (plot_type == "obs") {
    x <- fit$x
  } else {
    stop("plot_type not found")
  }

  # Color by classification
  n <- nrow(x)
  m <- max(comp)
  cols <- rainbow(m)[comp]
  cols[is.na(comp)] <- "grey"

  # Set color and type for selected rows
  cols[row_sel] <- "black"

  pchs <- rep(pch, n)
  pchs[row_sel] <- 4

  # Get order to draw selected rows last
  rows_selected <- (seq_len(n) %in% row_sel)
  o <- order(rows_selected)

  # Do plot
  plot(x = x[o, i],
       y = x[o, j],
       xlab = colnames(fit$u)[i],
       ylab = colnames(fit$u)[j],
       axes = FALSE,
       main = "",
       col = cols[o],
       pch = pchs[o],
       asp = 1)
  axis(1)
  axis(2)
}
