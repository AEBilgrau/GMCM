#' Fast optimization of Gaussian Mixture Copula Models
#'
#' Gaussian mixture copula models (GMCM) can be used for unsupervised 
#' clustering and meta analysis. In meta analysis, GMCMs are used to identify 
#' features which have been reproduced across multiple experiments. This 
#' package provides a fast and general implementation of GMCM cluster analysis 
#' and serves as an extension of the features available in the \code{idr} 
#' package.
#' 
#' @name GMCM-package
#' @aliases GMCM-package GMCM
#' @details If the meta analysis of Li et al. (2011) is to be performed, the 
#'   function \code{\link{fit.meta.GMCM}} is used to identify the maximum 
#'   likelihood estimate of the special Gaussian mixture copula model (GMCM) 
#'   defined by Li et al. (2011). The function \code{\link{get.IDR}} 
#'   computes the local and adjusted Irreprodcible Discovery Rates defined 
#'   by Li et al. (2011) to determine the level of reproduciblity.
#'   
#'   Tewari et. al. (2011) proposed using GMCMs as an general unsupervised 
#'   clustering tool. If such a general unsupervised clustering is needed, like 
#'   above, the function \code{\link{fit.full.GMCM}} computes the maximum 
#'   likelihood estimate of the general GMCM. The function 
#'   \code{\link{get.prob}} is used to estimate the class membership 
#'   probabilities of each observation.
#'   
#'   \code{\link{SimulateGMCMData}} provide easy simulation from the GMCMs.
#' 
#' @author 
#'   Anders Ellern Bilgrau, 
#'   Martin Boegsted, 
#'   Poul Svante Eriksen
#' 
#'   Maintainer: Anders Ellern Bilgrau <abilgrau@@math.aau.dk>
#' @docType package
#' @references
#'   Li, Q., Brown, J. B. J. B., Huang, H., & Bickel, P. J. (2011). 
#'   Measuring reproducibility of high-throughput experiments. The Annals of 
#'   Applied Statistics, 5(3), 1752-1779. doi:10.1214/11-AOAS466
#' 
#'   Tewari, A., Giering, M. J., & Raghunathan, A. (2011). Parametric 
#'   Characterization of Multimodal Distributions with Non-gaussian Modes. 
#'   2011 IEEE 11th International Conference on Data Mining Workshops, 
#'   286-292. doi:10.1109/ICDMW.2011.135
#' @seealso
#'   Core user functions: \code{\link{fit.meta.GMCM}}, 
#'   \code{\link{fit.full.GMCM}}, \code{\link{get.IDR}}, 
#'   \code{\link{get.prob}}, \code{\link{SimulateGMCMData}}, 
#'   \code{\link{SimulateGMMData}}, \code{\link{rtheta}}, 
#'   \code{\link{Uhat}}, \code{\link{choose.theta}}, 
#'   \code{\link{full2meta}}, \code{\link{meta2full}}
#' 
#'   Package by Li et. al. (2011): \code{\link[idr:idr-package]{idr}}.
#' @examples
#' # Loading data
#' data(u133VsExon)
#' 
#' # Subsetting data to reduce computation time
#' u133VsExon <- u133VsExon[1:5000, ]
#' 
#' # Ranking and scaling,
#' # Remember large values should be critical to the null!
#' uhat <- Uhat(1 - u133VsExon)
#' 
#' # Visualizing P-values and the ranked and scaled P-values
#' \dontrun{
#' par(mfrow = c(1,2))
#' plot(u133VsExon, cex = 0.5, pch = 4, col = "tomato", main = "P-values"
#'      xlab = "P   (U133)", ylab = "P   (Exon)"
#' plot(uhat, cex = 0.5, pch = 4, col = "tomato", main = "Ranked P-values",
#'      xlab = "rank(1-P)   (U133)", ylab = "rank(1-P)   (Exon)"
#' }
#'
#' # Fitting using BFGS
#' fit <- fit.meta.GMCM(uhat, init.par = c(0.5, 1, 1, 0.5), pgtol = 1e-2,
#'                      method = "L-BFGS", positive.rho = TRUE, verbose = TRUE)
#' 
#' # Compute IDR values and classify
#' idr <- get.IDR(uhat, par = fit)
#' table(idr$K) # 1 = irreproducible, 2 = reproducible
#' 
#' \dontrun{
#' # See clustering results
#' par(mfrow = c(1,2))
#' plot(u133VsExon, cex = 0.5, pch = 4, main = "Classified genes",
#'      col = c("tomato", "steelblue")[idr$K],
#'      xlab = "P   (U133)", ylab = "P   (Exon)")
#'   plot(uhat, cex = 0.5, pch = 4, main = "Classified genes",
#'        col = c("tomato", "steelblue")[idr$K],
#'        xlab = "rank(1-P)   (U133)", ylab = "rank(1-P)   (Exon)")
#' }
NULL


#' Reproduciblity between Affymetrix HG-U133 plus 2.0 and Human Exon 1.0 ST 
#' microarrays
#' 
#' This dataset contains two paired lists (columns) of unadjusted P-values for 
#' differential expression between germinal center cells and other B-cells 
#' within tonsils for two different experiments. The experiments differ 
#' primarily in the microarray platform used. The first column corresponds the 
#' evidence from the Affymetrix GeneChip Human Genome U133 Plus 2.0 Array. 
#' The second column corresponds to the Affymetrix GeneChip Human Exon 1.0 ST 
#' Array.
#' @docType data
#' @name u133VsExon
#' @details Further details can be found in Bilgrau et al. (2014). [...]
#' @format The format of the data is:
#' 
#'   \code{'data.frame':  19577 obs. of  2 variables:}\cr
#'   \code{$ u133: num  0.17561 0.00178 0.005371 0.000669 0.655261 ...}\cr
#'   \code{$ exon: num  1.07e-01 6.74e-10 1.51e-03 6.76e-05 3.36e-01 ...}\cr
#' @author Anders Ellern Bilgrau <abilgrau (at) math.aau.dk>
#' @references Fill in!
#' @keywords datasets, data
#' @examples
#' data(u133VsExon)
#' str(u133VsExon)
#' 
#' # Plot P-values
#' plot(u133VsExon, cex = 0.5)
#' 
#' # Plot ranked and scaled P-values
#' plot(Uhat(1-u133VsExon), cex = 0.5) 
NULL