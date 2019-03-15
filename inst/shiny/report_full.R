#' ---
#' title: "Unsupervised clusering with general GMCMs"
#' output: html_document
#' date: '`r Sys.time()`'
#' author: "by the GMCM shiny app"
#' params:
#'   file: "C:/Users/ander/Documents/Rprog/GMCM/data/freshVsFrozen.csv"
#'   header: TRUE
#'   sep: ";"
#'   quote: '"'
#' ---


#' ## Initalisation
#' The needed packages are loaded.
# ---- load-packages, include=TRUE

#install.packages("GMCM")  # Uncomment to install the GMCM package
library("GMCM")

#' If **GMCM** is not installed, please uncomment the above line and rerun the script.
#' This document is parameterized with the following parameters:
# ---- params, results='hold', comment="", echo=FALSE
cat("params <- ")
cat(capture.output(dput(params)), sep = "\n")




#' ## Load data
#' The data is loaded and the first rows are printed
# ---- load-data, include=TRUE, echo=TRUE
ds <- read.table(file   = params$file,
                 header = params$header,
                 sep    = params$sep,
                 quote  = params$quote)
head(ds)



#' ## Model fitting
#' With the data loaded, the model can now be fitted.
#' However, some inital parameters are needed

#' ## Unsupervised clustering


#' ## Results












#' ## References

# ---- citation, echo=FALSE, results='asis'
cat("1. ", format(citation("GMCM"), style = "text"))

#' ## Session information

# ---- session-info
sessionInfo()
