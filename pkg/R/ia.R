#' Whittaker's index of association
#' 
#' Creates distance matrix based on Whittaker's index of association, which is reflects the distances between weighted means of species attributes calculated for individual samples. The metric is baselly Manhattan-type distance calculated on species profiles (i.e. on community matrix in which individual rows are standardized to row-totals).
#' @param sitspe Compositional matrix (samples x species).
#' @author David Zeleny (zeleny.david@@gmail.com)
#' @references
#' Legendre P. & Legendre L. 2012. Numerical Ecology. 3rd edn. Elsevier, Oxford, UK. 
#' @export
ia <- function (sitspe) vegdist (decostand (sitspe, 'total'), 'manhattan')/2