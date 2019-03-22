library("knitr")
library("rmarkdown")

# Run directly
report_path <- "inst/shiny/www/report_full.R"
rmarkdown::render(report_path,
                  envir = new.env(parent = globalenv()))


# Step-by-step
# Spin document into Rmd
rmd <- spin(hair = report_path, knit = FALSE, format = "Rmd")

# Read yaml
yaml <- rmarkdown::yaml_front_matter(rmd)
params <- yaml$params


rmarkdown::render(
  input = rmd,
  output_options = list(self_contained = TRUE),
  params = params,
  envir = new.env(parent = globalenv())
)

# We can also purl
knitr::purl(rmd, output = file.path(dirname(rmd), "report_full_purl.R"), documentation = 0)
