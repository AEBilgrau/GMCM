
# Function for plotting meta results
meta_plot <- function(fit, # A fitted object data
                      idr, # A
                      plot_type = c("rank", "gmm"),
                      col_sel,
                      row_sel,
                      col = c(rep = "steelblue", irrep = "#00000050", sel = "red"),
                      pch = c(rep = 1, irrep = 1, sel = 16)) {

  col_sel <- setdiff(col_sel, 0) # Exclude index 0 (rownames)

  if (is.null(col_sel) || length(col_sel) != 2) {
    i <- 1
    j <- 2
  } else {
    i <- col_sel[1]
    j <- col_sel[2]
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
