#' @importFrom utils write.csv2
run_meta_report <- function(...) {

  # Simulate some data and put it into a CSV file
  sim_data <- SimulateGMCMData(n = 1000, par = c(pie1 = 0.3, mu = 2,
                                                 sigma = 1.1, rho = 0.2))$z
  colnames(sim_data) <- c("u133", "exon")
  data_file <- tempfile()
  utils::write.csv2(sim_data, file = data_file)


  # GENERAL GMCM ----
  report_path_full <- system.file("shiny", "www", "report_full.Rmd",
                                  package = "GMCM", lib.loc = .libPaths()[1],
                                  mustWork = TRUE)

  # Parameters to expand
  params <- list(data_file = data_file,
                 header = TRUE,
                 sep = ";",
                 quote = '\"',
                 model_cols = c("u133", "exon"),
                 theta = GMCM::rtheta(m = 2, d = 2),
                 fit_method = "NM",
                 max_ite = 50,
                 full_class_type = "thres_prob",
                 full_thres_prob = 0.9)

  expand_args <- c(list(file = report_path_full), params)
  report_expanded <- do.call(knitr::knit_expand, expand_args)
  report_expanded_path <- gsub("/report_", "/report_expanded_", report_path_full)

  cat(report_expanded, file = report_expanded_path)

  # We can purl
  res3 <- knitr::purl(report_expanded_path,
                      output = gsub(".Rmd$", ".R", report_expanded_path),
                      documentation = 0)

  # Render the expanded document
  res4 <- rmarkdown::render(
    input = report_expanded_path,
    output_options = list(self_contained = TRUE),
    envir = new.env(parent = globalenv())
  )

  return(invisible(list(report_expanded_path, res3, res4)))
}
