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
#'   model_cols: !r c("PreVsPost.Fresh.pval", "PreVsPost.Frozen.pval")
#'   theta: !r GMCM::rtheta(m = 2, d = 2)
#'   fit_method: "NM"
#'   max_ite: 50
#'   full_class_type: "thres_prob"
#'   full_thres_prob: 0.9
#' ---

#'
# ---- knit-int, echo=FALSE, include=FALSE
set.seed(7869670)

#'
#' ## Initalisation
#' The needed packages are loaded.
# ---- load-packages, include=TRUE

#install.packages("GMCM")  # Uncomment to install the GMCM package
library("GMCM")

#'
#' If **GMCM** is *not* installed, please uncomment the above line and rerun the script.
#'
#' ## Load data
#' The data is loaded and the first rows are printed
# ---- load-data, include=TRUE, echo=TRUE
ds <- read.table(file   = params$file,
                 header = params$header,
                 sep    = params$sep,
                 quote  = params$quote)
head(ds, n = 4)

#' Next, the columns of interest are selected and the data subsetted
# ---- select-data, include=TRUE, echo=TRUE
x <- ds[, params$model_cols]
head(x, n = 2)

#'
#' ## Initial parameters
#' The inital parameters are given by
# ---- show-initial-params, include=TRUE, echo=TRUE
theta <- as.theta(params$theta)
print(theta)


#'
#' ## Model fitting
#' With the data loaded and defined initial parameters, the model is now fitted.
# ---- fit_model, error=TRUE
theta <- fit.full.GMCM(u = x,  # Ranking function is applied automatically
                       theta = theta,
                       method = params$fit_method,
                       max.ite = params$max_ite,
                       verbose = FALSE)
print(theta)

#' The fitting method is set `r params$fit_method` with a maximum number of interations of `r params$max_ite`.
#'
#' ## Unsupervised clustering
#' The estimated parameters are used to calculated posterior component probabilities on which the classification is based:
# ---- compute_probs
kappa <- get.prob(x, theta)
comps <- apply(kappa, 1, which.max)

if (params$full_class_type == "thres_prob") {
  ok_max <- apply(kappa, 1, max) > params$full_thres_prob
  comps[!ok_max] <- NA
}

head(cbind(kappa, comp = comps))

#' ## Results
#' The results are displayed by plotting
# ---- plot_results
plot(x, col = comps)

#'
#' ## References

# ---- citation, echo=FALSE, results='asis'
cat("1. ", format(citation("GMCM"), style = "text"))

#' ### Session information
#' This report was generated using `rmarkdown` and `knitr` with the following session information.
# ---- session-info
sessionInfo()
