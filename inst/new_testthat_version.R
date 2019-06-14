rfiles <- list.files("tests", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
rfiles <- rfiles[rfiles != "xx.R"]

for (f in rfiles) {
  the_file <- readLines(f)
  o <- grep("expect_that(.*is_true())", the_file)
  the_file[o] <- gsub("expect_that(", "expect_true(", the_file[o], fixed = TRUE)
  the_file[o] <- gsub(",[[:space:]]*is_true\\(\\)\\)", ")", the_file[o])
  writeLines(the_file, con = f)
}
