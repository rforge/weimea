#' Regression of ordination scores with weighted mean of species attributes, tested by modified permutation test 
#' 
#' The function fits weighted mean of species attributes (e.g. mean Ellenberg indicator values), calculated for samples, onto an ordination. It employs modified permutation test, based on a null model randomizing species attributes prior to calculation of their sample mean weighted by their abundance. This test is invented to correct the bias in the relationship between mean species attributes (e.g. mean Ellenberg indicator values) and sample scores along ordination axes introduced by the fact that weighted mean of species attributes inherits compositional similarity among samples. 
#' @name envfit_wm
#' @param ord an ordination object of class \code{cca} created by some of the \code{vegan} functions (e.g. \code{\link{rda}}, \code{\link{cca}} or \code{\link{decorana}}) or other type of object from which the ordination scores can be extracted (including an object of the class \code{matrix} or \code{data.frame} with columns of ordination scores).
#' @param env object of the class \code{wm} with weighted means of species attributes
#' 
#' @author David Zeleny (zeleny.david@@gmail.com); the script is almost entirely based on the original functions envfit and vectorfit from \code{vegan} package, written by Jari Oksanen. 
#' @references Zeleny D. & Schaffers A.P. (2012): Too good to be true: pitfalls of using mean Ellenberg indicator values in vegetation analyses. Journal of Vegetation Science, 23: 419-431
#' @return Object of the \code{vegan} class \code{\link{envfit}}
#' @seealso \code{plot.envfit} for plotting the fitted vectors onto ordination diagram, \code{scores.envfit}, \code{print.envfit} 
#' @examples
#' library (vegan)
#' data (vltava.spe)
#' data (vltava.ell)
#' vltava.mean.eiv <- wm (sitspe = vltava.spe, speatt = vltava.ell)
#' dca <- decorana (vltava.spe)

#' # Calculate the fit of mean Ellenberg indicator values with significances based on the original permutation model:
#' fit.orig <- envfit (ord = dca, env = vltava.mean.eiv, permutations = 499)

#' # Calculate the fit of mean Ellenberg indicator values with modified permutation model (this expects that you defined the envfit.wm function):
#' fit.modif <- envfit_wm (ord = dca, env = vltava.mean.eiv, permutations = 499)

#' # Plot DCA ordination diagram with passively projected Ellenberg indicator values:
#' plot (dca, display = 'sites', type = 'p')
#' plot (fit.orig, p.max = .05)
#' plot (fit.modif, p.max = 0.05, col = 'red')
#' @export

envfit_wm <- function (ord, env, permutations = 999, choices = c(1, 2), display = "sites", w = weights(ord), na.rm = FALSE, ...)
{
  weights.default <- function(object, ...) NULL
  w <- eval(w)
  vectors <- NULL
  factors <- NULL
  seed <- NULL
  X <- scores(ord, display = display, choices = choices, ...)
  keep <- complete.cases(X)
  if (any(!keep)) {
    if (!na.rm) 
      stop("missing values in data: consider na.rm = TRUE")
    X <- X[keep, , drop = FALSE]
    na.action <- structure(seq_along(keep)[!keep], class = "omit")
  }
  vectors <- vectorfit_wm(X, env, permutations, choices,
                          w = w, ...)
  sol <- list(vectors = vectors, factors = factors)
  if (!is.null(na.action)) 
    sol$na.action <- na.action
  class(sol) <- "envfit"
  sol
}

#' @rdname envfit_wm
vectorfit_wm <- function (X, P, permutations, w, ...) 
{
  X <- as.matrix(X)
  if (missing(w) || is.null(w)) 
    w <- 1
  if (length(w) == 1) 
    w <- rep(w, nrow(X))
  Xw <- .C("wcentre", x = as.double(X), as.double(w), as.integer(nrow(X)), 
           as.integer(ncol(X)), PACKAGE = "vegan")$x
  dim(Xw) <- dim(X)
  Pw <- .C("wcentre", x = as.double(P), as.double(w), as.integer(nrow(P)), 
           as.integer(ncol(P)), PACKAGE = "vegan")$x
  dim(Pw) <- dim(P)
  colnames(Pw) <- colnames(P)
  nc <- ncol(X)
  Q <- qr(Xw)
  H <- qr.fitted(Q, Pw)
  heads <- qr.coef(Q, Pw)
  r <- diag(cor(H, Pw)^2)
  heads <- decostand(heads, "norm", 2)
  heads <- t(heads)
  if (is.null(colnames(X))) 
    colnames(heads) <- paste("Dim", 1:nc, sep = "")
  else colnames(heads) <- colnames(X)
  if (permutations) {
    nr <- nrow(X)
    permstore <- matrix(nrow = permutations, ncol = ncol(P))
    for (i in 1:permutations) {
      take <- randomize (P)
      take <- .C("wcentre", x = as.double(take), as.double(w), 
                 as.integer(nrow(take)), as.integer(ncol(take)), 
                 PACKAGE = "vegan")$x
      dim(take) <- dim(P)
      Hperm <- qr.fitted(Q, take)
      permstore[i, ] <- diag(cor(Hperm, take))^2
    }
    permstore <- sweep(permstore, 2, r, ">")
    pvals <- (apply(permstore, 2, sum) + 1)/(permutations + 1)
  }
  else pvals <- NULL
  sol <- list(arrows = heads, r = r, permutations = permutations, 
              pvals = pvals)
  class(sol) <- "vectorfit"
  sol
}