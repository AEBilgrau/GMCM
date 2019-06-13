# dput object to character string
# cput is used inside the reports
cput <- function(x) {
  f <- tempfile()
  dput(x, file = f)
  return(readLines(f))
}
