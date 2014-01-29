\name{u133VsExon}
\alias{u133VsExon}
\docType{data}
\title{
Reproduciblity between Affymetrix HG-U133 plus 2.0 and Human Exon 1.0 ST microarrays
}
\description{
This dataset contains two paired lists (columns) of unadjusted P-values for differential expression between germinal center cells and other B-cells within tonsils for two different experiments. The experiments differ primarily in the microarray platform used. The first column corresponds the evidence from the Affymetrix GeneChip Human Genome U133 Plus 2.0 Array. The second column corresponds to the Affymetrix GeneChip Human Exon 1.0 ST Array.
}
\usage{data(u133VsExon)}
\format{
The format of the data is:\cr
'data.frame':  19577 obs. of  2 variables: \cr
 $ u133: num  0.17561 0.00178 0.005371 0.000669 0.655261 ...\cr
 $ exon: num  1.07e-01 6.74e-10 1.51e-03 6.76e-05 3.36e-01 ...
}
\details{
Further details can be found in Bilgrau et al. (2013). [...]
}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(u133VsExon)
str(u133VsExon)

# Plot P-values
plot(u133VsExon, cex = 0.5)

# Plot ranked and scaled P-values
plot(Uhat(1-u133VsExon), cex = 0.5)
}
\keyword{datasets}
