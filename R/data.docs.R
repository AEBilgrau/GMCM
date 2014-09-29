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
#' @details Further details can be found in Rasmussen and Bilgrau et al. (2014).
#' @format The format of the data is: \cr
#'   \code{'data.frame':  19577 obs. of  2 variables:}\cr
#'   \code{$ u133: num  0.17561 0.00178 0.005371 0.000669 0.655261 ...}\cr
#'   \code{$ exon: num  1.07e-01 6.74e-10 1.51e-03 6.76e-05 3.36e-01 ...}\cr
#' @author Anders Ellern Bilgrau <abilgrau (at) math.aau.dk>
#' @references
#'   Rasmussen SM, Bilgrau AE, Schmitz A, Falgreen S, Bergkvist KS, Tramm AM,
#'   Baech J, Jacobsen CL, Gaihede M, Kjeldsen MK, Boedker JS, Dybkaer K,
#'   Boegsted M, Johnsen HE (2014).
#'   "Stable Phenotype Of B-Cell Subsets Following Cryopreservation and Thawing
#'   of Normal Human Lymphocytes Stored in a Tissue Biobank."
#'   Cytometry Part B: Clinical Cytometry. ISSN 1552-4957.
#'   doi:10.1002/cytob.21192. URL http://dx.doi.org/10. 1002/cytob.21192.
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


#' Reproduciblity between Fresh and Frozen B-cell subtypes
#'
#' This dataset contains two paired lists (columns) of t-scores (from a Linear
#' mixed effects model) for
#' differential expression between pre (Im, N) and post germinal (M, PB) centre
#' cells within peripheral blood.
#' The first column contain the t-scores for freshly sorted and gene profiled
#' cells.
#' The second column contain the t-scores for cryopreserved (frozen), thawed,
#' sorted and gene profiled cells.
#' The used array type was Affymetrix Human Exon 1.0 ST microarray.
#'
#' @docType data
#' @name freshVsFrozen
#' @details Further details can be found in Rasmussen and Bilgrau et al. (2014).
#' @format The format of the data is:\cr
#'  \code{'data.frame':  18708 obs. of  2 variables:}\cr
#'  \code{$ Fresh : num  -1.073 -0.381 -1.105 -0.559 -1.054 ...}\cr
#'  \code{$ Frozen: num  -0.245 -0.731 -0.828 -0.568 -1.083 ...}\cr
#' @author Anders Ellern Bilgrau <abilgrau (at) math.aau.dk>
#' @references
#'   Rasmussen SM, Bilgrau AE, Schmitz A, Falgreen S, Bergkvist KS, Tramm AM,
#'   Baech J, Jacobsen CL, Gaihede M, Kjeldsen MK, Boedker JS, Dybkaer K,
#'   Boegsted M, Johnsen HE (2014).
#'   "Stable Phenotype Of B-Cell Subsets Following Cryopreservation and Thawing
#'   of Normal Human Lymphocytes Stored in a Tissue Biobank."
#'   Cytometry Part B: Clinical Cytometry. ISSN 1552-4957.
#'   doi:10.1002/cytob.21192. URL http://dx.doi.org/10. 1002/cytob.21192.
#' @keywords datasets, data
#' @examples
#' data(freshVsFrozen)
#' str(freshVsFrozen)
#'
#' # Plot P-values
#' plot(freshVsFrozen, cex = 0.5)
#'
#' # Plot ranked and scaled P-values
#' plot(Uhat(abs(freshVsFrozen)), cex = 0.5)
NULL
