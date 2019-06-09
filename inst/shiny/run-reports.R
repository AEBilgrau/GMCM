library("knitr")
library("rmarkdown")
library("GMCM")

# dput object to character string
cput <- function(x) {
  f <- tempfile()
  dput(x, file = f)
  return(readLines(f))
}

# Simulate some data and put it into a CSV file
sim_data <- SimulateGMCMData(n = 1000, par = c(pie1 = 0.3, mu = 2, sigma = 1.1, rho = 0.2))$z
colnames(sim_data) <- c("u133", "exon")
data_file <- tempfile()
write.csv2(sim_data, file = data_file)


# SPECIAL GMCM ----
report_path <- system.file("shiny/www/report_meta.Rmd", package = "GMCM")

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

expand_args <- c(list(file = report_path), params)
report_expanded <- do.call(knitr::knit_expand, expand_args)
report_expanded_path <- gsub("/report_", "/report_expanded_", report_path)

# Write to file
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

print(res2)


# GENERAL GMCM ----
report_path <- system.file("shiny/www/report_full.Rmd", package = "GMCM")

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

expand_args <- c(list(file = report_path), params)
report_expanded <- do.call(knitr::knit_expand, expand_args)
report_expanded_path <- gsub("/report_", "/report_expanded_", report_path)

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

print(res4)
