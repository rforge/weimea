#' Weighted mean of species attributes
#' 
#' Function \code{wm} calculates weighted mean of species attributes, using matrices of species composition and species attributes. Other functions are for handling the objects of class \code{wm}.
#' 
#' @param sitspe Matrix or data.frame with community data matrix (sites x species)
#' @param speatt Vector, matrix or data.frame with species attributes (species x attributes)
#' @param object any R object
#' @param M Object of the class \code{wm}
#' @param drop  In function \code{[.wm]} if \code{TRUE} coerces the result to the lowest possible dimension (i.e. vector if matrix has only one column). Currently not implemented, change into \code{TRUE} will have no effect.
#' @param i,j Subscripts of the matrix of the class "Wm" (rows and columns).
#' @details
#' Function \code{[.wm]} is for extracting specified rows and columns from matrix of class \code{wm}. As a side effect, resulting object will have concatenated the \code{sitspe} and \code{speatt} attributes to match the dimension of the resulting matrix. This function is only for extracting the parts of \code{wm} object, not for replacing! (attempt to replace will work, but will break the inner structure).
#' @return Object of class \code{\link{wm}}, which contains the matrix of calculated weighted means of species attributes for each sample (sample x weighted mean) and two attributes: \code{sitspe} species x sample matrix from which the weighted mean was calculated, and \code{speatt} species x attributes matrix with species attributes. All weighted means of species attributes must be based on the same species x sample matrix with the same number of samples.
#' @examples
#' # Calculation of weighted mean of species Ellenberg indicator values using dataset Vltava
#' data (vltava)
#' mean.eiv <- wm (sitspe = vltava$spe, speatt = vltava$ell)
#' 
#' summary (mean.eiv)
#' 
#' # Extracting values from the object of \code{wm} class
#' cwm[,1]
#' cwm[1:10, 2:3]
#' @author David Zeleny (zeleny.david@@gmail.com)
#' @export
#' 
wm <- function (sitspe, speatt)
{
  sitspe <- as.matrix (sitspe)
  speatt <- as.matrix (speatt)
  wm.temp <- apply (speatt, 2, FUN = function (x) vegan:::decostand (sitspe[,!is.na (x)], 'total') %*% x[!is.na(x)])
  attr (wm.temp, 'sitspe') <- sitspe
  attr (wm.temp, 'speatt') <- speatt
  attr(wm.temp, 'class') <- c('wm')
  wm.temp
}


#' @rdname wm
#' @export
is.wm <- function (object)
{
  if (any (class (object) == 'wm') & !is.null (attr (object, 'sitspe'))  & !is.null (attr (object, 'speatt'))) TRUE else FALSE
}


#' @rdname wm
#' @export
"[.wm" <- function (M, i, j, drop = F)
{
  if (missing (i)) i <- 1:nrow (M)
  if (missing (j)) j <- 1:ncol (M)
  sitspe <- attr (M, 'sitspe')
  speatt <- attr (M, 'speatt')
  M <- as.matrix (M)
  res <- M[i, j, drop = F]
  attr (res, 'sitspe') <- sitspe [i,, drop = F]
  attr (res, 'speatt') <- speatt [,j, drop = F]
  class (res) <- c('wm')
  res
}

#' @rdname wm
#' @export
as.matrix.wm <- function (M)
{
  attr (M, 'sitspe') <- NULL
  attr (M, 'speatt') <- NULL
  M <- unclass (M)
  M <- as.matrix (M)
  return (M)
}

#' @rdname wm
#' @param long should summary return long output? (TRUE vs FALSE)
#' @export
summary.wm <- function (M, long = F)
{
  sitspe <- attr (M, 'sitspe')
  speatt <- attr (M, 'speatt')
  cat ("Object of the class 'wm'\n")
  cat ("\nWeighted means:     \t\t", dim (M), "\t(sites x variables)")
  cat ("\nSpecies composition:\t\t", dim (sitspe), "\t(sites x species), \trange of values:", range (sitspe, na.rm = T))
  cat ("\nSpecies attributes: \t\t", dim (speatt), "\t(species x attributes)")
  na <- apply (speatt, 2, FUN = function (x) sum (is.na (x)))
  if (sum (na) > 0)  cat ("\n\t\tMissing values of sp. attributes:\t", paste (names (na), na, sep = ' ', col = '\t'))
  if (long)
  {
    cat ("\n\nSummary of wm matrix:\n\n")
    print (summary (as.matrix (M)))
    cat ("\nSummary of community matrix\n\n")
    print (summary (attr (M, 'sitspe')))
    cat ("\nSummary of species attributes\n\n")
    print (summary (attr (M, 'speatt')))
  }
}

#' @rdname wm
#' @export
print.wm <- function (M)
{
  attr (M, 'sitspe') <- NULL
  attr (M, 'speatt') <- NULL
  class (M) <- 'matrix'
  print (as.matrix (M))
}