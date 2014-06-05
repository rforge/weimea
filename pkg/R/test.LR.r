#' Test of the link between sample attributes and matrix of species composition (i.e. whether \bold{L} is linked to \bold{R})
#' 
#' The function testing the link between sample attributes and species composition of the matrix, from which weighted means are calculated. The test is based on db-RDA (distance-based redundancy analysis), and is using matrix of intersample distances calculated using Whittaker's index of association (\code{\link{ia}}). Significance of the variation explained by sample attributes (R2) is tested by Monte Carlo permutation test. Significant relationship is considered as an argument to use modified permutation test instead of the standard permutation test for testing the relationship between weighted mean of species attributes and sample attributes.
#' @param M Object of the 'wm' class. Matrix with weighted means of species attributes.
#' @param env Matrix with environmental variables.
#' @param type Currently not implemented. In the future, other types of the test (apart to the one based on db-RDA) should be available.
#' @param alpha Target Type I error rate for Monte Carlo permutation tests (influences number of permutations).
#' @param sqrt Logical value, default FALSE. Should the distance matrix based on Whittaker's index of association be square-rooted to become Euclidean? See Details.
#' 
#' @details
#' Whittaker's index of association (calculated as Manhattan type distance on species profiles) is metric, but not Euclidean, and in PCoA (on which dbRDA is based) it can produced negative eigenvalues. After square root transformation, the index becomes both metric and Euclidean.
#'
#' @return
#' List of lists, each node containing two parts: results of dbRDA analysis (calculated by \code{\link{capscale}} function from \code{vegan}) and results of Monte Carlo permutation test (calculated by \code{\link{anova.cca}} function, also from \code{vegan}). 
#' @name test.LR
#' @examples
#' data (vltava)
#' test.LR (M = wm (vltava$spe, vltava$ell), vltava$env, alpha = 0.05)

#' @export
test.LR <- function (M, env, type = 'dbRDA', alpha = 0.001, sqrt = F)
{
  if (!is.wm (M)) stop ("Argument M must be an object of class 'wm'!")
  env <- as.matrix (env)
  if (is.null(colnames (env))) colnames (env) <- 'env'
  res <- list ()
  for (co.M in colnames (M))
   for (co.env in colnames (env))
     res [[co.M]][[co.env]] <- test.LR.0 (M = M[,co.M], env = env[,co.env], type = type, alpha = alpha, sqrt = sqrt)
  class (res) <- 'testLR'
  return (res)
}

test.LR.0 <- function (M, env, type = 'dbRDA', alpha = 0.001, sqrt = F)
{
  sitspe <- attr (M, 'sitspe')
  speatt <- attr (M, 'speatt')
  sitspe.temp <- sitspe[, !is.na (speatt)]
  if (sqrt) pcoa.temp <- capscale (sqrt (ia (sitspe.temp)) ~ env) else pcoa.temp <- capscale (ia (sitspe.temp) ~ env)
  anova.temp <- anova (pcoa.temp, alpha = alpha)
  res <- list (pcoa = pcoa.temp, anova = anova.temp)
  return (res)
}

#' @rdname test.LR
#' @export
print.testLR <- function (object, digits = 3)
{
  symnum.pval <- function (pval) symnum( pval, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " "))
  names.speatt <- names (object)
  names.env <- names (object[[1]])
  res <- lapply (object, FUN = function (sp) lapply (sp, FUN = function (en) c(format (RsquareAdj (en$pcoa)$r.squared, digits = digits), symnum.pval (en$anova[,'Pr(>F)'][1]))))
  res.m <- matrix (unlist (res), ncol = 2*length (names.env), nrow = length (names.speatt), dimnames = list (names.speatt, as.vector (rbind (names.env, ""))), byrow = T)
  cat ('\nTable of variation in species composition, which is used to calculate individual weighted means of species attributes (in rows), explained by explanatory variables (in columns). Explained variation is expressed as R2 (not adjusted).\n\n')
  print.default (res.m, quote = F, right = T)
}

#' @rdname test.LR
summary.testLR <- function (object)
  print.default (object)