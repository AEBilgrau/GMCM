#' @importFrom utils write.csv2
run_full_report <- function(...) {

  # Simulate some data and put it into a CSV file
  sim_data <- SimulateGMCMData(n = 1000, par = c(pie1 = 0.3, mu = 2,
                                                 sigma = 1.1, rho = 0.2))$z
  colnames(sim_data) <- c("u133", "exon")
  data_file <- tempfile()
  utils::write.csv2(sim_data, file = data_file)


  # SPECIAL GMCM ----
  report_path_meta <- system.file("shiny", "www", "report_meta.Rmd",
                                  package = "GMCM", lib.loc = .libPaths()[1],
                                  mustWork = TRUE)

  # Parameters to expand
  params <- list(data_file = data_file,
                 header = TRUE,
                 sep = ";",
                 quote = '\"',
                 model_cols = c("u133", "exon"),
                 meta_large_vals = FALSE,
                 init_par = c(pie1 = 0.5, mu = 1, sigma = 1, rho = 0.5),
                 meta_method = "NM",
                 meta_max_ite = 50,
                 meta_positive_rho = TRUE,
                 meta_IDR_thres_type = "IDR",
                 meta_IDR_thres = 0.01)

  expand_args <- c(list(file = report_path_meta), params)
  report_expanded <- do.call(knitr::knit_expand, expand_args)

  # Write to file
  report_expanded_path <- tempfile()
  cat(report_expanded, file = report_expanded_path)

  # We can also purl
  res1 <- knitr::purl(report_expanded_path,
                      output = gsub(".Rmd$", ".R", report_expanded_path),
                      documentation = 0)

  # Render the expanded document
  res2 <- rmarkdown::render(
    input = report_expanded_path,
    output_options = list(self_contained = TRUE),
    envir = new.env(parent = globalenv())
  )

  return(invisible(list(report_expanded_path, res1, res2)))
}
