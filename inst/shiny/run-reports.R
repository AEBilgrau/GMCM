library("knitr")
library("rmarkdown")
library("GMCM")

# GENERAL GMCM ----
report_path <- "inst/shiny/www/report_full.Rmd"

# SPECIAL GMCM ----
report_path <- "inst/shiny/www/report_meta.Rmd"

# Parameters to expand
params <- list(input_file = "../../../data/u133VsExon.csv",
               header = TRUE,
               sep = ";",
               quote = '\"',
               model_cols = c("u133", "exon"),
               meta_large_vals = FALSE,
               init_par = c(pie1 = 0.5, mu = 1, sigma = 1, rho = 0.5),
               meta_method = "NM",
               meta_max_ite = 50L,
               meta_positive_rho = TRUE,
               meta_IDR_thres_type = "IDR",
               meta_IDR_thres = 1e-04)

# dput object to character string
cput <- function(x) {
  f <- tempfile()
  dput(x, file = f)
  return(readLines(f))
}


expand_args <- c(list(file = report_path), params)
report_expanded <- do.call(knitr::knit_expand, expand_args)
report_expanded_path <- gsub("/report_", "/report_expanded_", report_path)

cat(report_expanded, file = report_expanded_path)

# Render the expanded document
rmarkdown::render(
  input = report_expanded_path,
  output_options = list(self_contained = TRUE),
  params = params,
  envir = new.env(parent = globalenv())
)

# We can also purl
knitr::purl(report_expanded_path, output = gsub(".Rmd$", ".R", report_expanded_path),
            documentation = 0)


