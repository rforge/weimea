#' Testing the relationship between weighted-mean of species attributes with sample attributes
#' 
#' Function calculating relationship between weighted-mean of species attributes and sample attributes and performing standard (row based), modified (column based), or max test of significance.
#' 
#' @name test_cwm
#' @param cwm An object of the class \code{cwm}. 
#' @param env Vector or matrix with variables. See details.
#' @param method Statistical method used to analyse the relationship between cwm (of class \code{cwm}) and env (sample attributes); partial match to \code{'cor'}, \code{'lm'} and \code{'aov'}. 
#' @param wcor Logical; should the correlation be weighted by rowsums of \code{com} (extracted from \code{cwm}?). Default \code{FALSE}.
#' @param wstand Logical; should the variables in correlation be first weighted-standardized? Default \code{FALSE}. If \code{wstand = TRUE}, both \code{env} and \code{traits} are weighted standardized prior to calcuation; weights are derived from \code{com} matrix (extracted from \code{cwm}); \code{env} is weighted by rowsums of \code{com}, while \code{traits} are weighted by colsums of \code{com}.
#' @param wreg \code{NULL} (default) or numeric vector of weights used in regression (\code{method = 'lm'}). If weights are provided, weighted least squares are calculated instead of ordinary least squares.
#' @param dependence Should \code{cwm} be dependent variable and \code{env} independent (\code{'cwm ~ env'}), or opposite? Applicable only for \code{method = 'lm'}. Partial match to \code{'cwm ~ env'} and \code{'env ~ cwm'}.
#' @param perm Number of permutations.
#' @param test Vector of character values. Which test should be conducted? Partial match to \code{'standard'} or \code{'rowbased'} for standard (row based) permutation test, \code{'modified'} or \code{'colbased'} for modified (column based) permutation test, \code{'max'} for max test (selecting the higher from \code{rowbased} and \code{colbased} result), and \code{'all'} including all three tests. See \code{Details}.
#' @param adjustP Logical, default FALSE. Should be the P-values adjusted? If \code{adjustP = TRUE}, the last column in the results is adjusted by method selected in \code{p.adjust.method}.
#' @param p.adjust.method A string indicating the method of P-value adjustement, see \code{\link{p.adjust.methods}} for possible choices.
#' @param parallel NULL (default) or integer number. Number of cores for parallel calculation of modified permutation test. Maximum number of cores should correspond to number of available cores on the processor.
#' @param x,object object of the class \code{"cwm"}, generated by function \code{cwm}.
#' @param digits number of digits reported by \code{print} method on object of \code{"cwm"} class (default is 3).
#' @param missing.summary Logical; should be the summary of values missing in \code{env}, \code{cwm} and \code{traits} be printed along to the result of \code{test_cwm} analysis? Default is \code{TRUE}.
#' @param alpha,line,cex.lab,par.mar,box.col,box.lwd Graphical parameters for \code{plot} function.
#' @param ... Other arguments for \code{print}, \code{summary}, \code{coef} or \code{plot} functions (some not implemented yet).

#' @export
#' @examples 
#' data (vltava)
#' CWM <- cwm (com = vltava$herbs$spe, traits = vltava$herbs$traits)
#' re <- test_cwm (cwm = CWM, env = vltava$env[,c('pH', 'COVERE32')])
#' re
#' plot (re)
#' @details
#' Currently implemented statistical methods: \code{'cor'}. Plan to implement also: \code{'lm'} and \code{'aov'}. For fourth corner, please use \code{\link{test_fourth}}.
#'
#' Argument \code{env} can be vector or matrix with one column. Only in the case of linear regression (\code{method = 'lm'}) it is possible to use matrix with several variables, which will all be used as independent variables in the model. For ANOVA and Kruskal-Wallis test, make sure that 'env' is \code{factor} (warning will be returned if this is not the case, but the calculation will be conducted). 
#' 
#' Difference between \code{method = 'lm'} and \code{'aov'} is in the format of summary tables, returned by \code{summary.cwm} function. In case of 'aov', this summary is expressed in the traditional language of ANOVA rather than linear models.
#' 
#' Both \code{method = 'lm'} and \code{'slope'} are based on linear regression and calculated by function \code{\link{lm}}, but differ by test statistic: while 'lm' is using F value and is testing the strength of the regression (measured by r2), 'slope' is using the slope of the regression line (b). This statistic is added here for comparison with the fourth corner method.
#' 
#' Specific issue related to weighted mean is the case of missing species attributes. In current implementation, species with missing species attributes are removed from sample x species matrix prior to permutation of species attributes among species. 
#' @return  Function \code{cwm} returns list of the class \code{"cwm"} (with \code{print} and \code{summary} methods), which contains the following components:
#' \itemize{
#'  \item \code{call} Call to the function.
#'  \item \code{out} Matrix with analysis results (coefficients, statistics, P-values).
#'  \item \code{miss} Matrix with counts of missing values in \code{env}, \code{cwm} and \code{traits}.
#'  \item \code{param} List with the setting of the function parameters (arguments).
#' }
#' @seealso \code{\link{cwm}}, \code{\link{snc}}
#' @importFrom graphics box par plot title 
#' @export
test_cwm <- function (cwm, env, method = c('cor'), wcor = FALSE, wstand = FALSE, wreg = NULL, dependence = "cwm ~ env", perm = 499, test = "max", parallel = NULL, p.adjust.method = 'holm', adjustP = FALSE)
{
  CALL <- match.call ()
  METHOD <- c('cor', 'lm', 'aov')
  TEST <- c('none', 'parametric', 'standard', 'rowbased', 'modified', 'colbased', 'max', 'all')
  DEPENDENCE <- c("cwm ~ env", "env ~ cwm")
  p.adjust.method <- match.arg (p.adjust.method, p.adjust.methods)
  method <- match.arg (method, METHOD)
  test <- match.arg (test, TEST, several.ok = T)
  if ('none' %in% test) test <- 'none'
  if ('all' %in% test) test <- c('parametric', 'rowbased', 'colbased', 'max')
  dependence <- match.arg (dependence, DEPENDENCE)
  
  if (!is.cwm (cwm) & ("modified" %in% test || "colbased" %in% test || "max" %in% test || "all" %in% test)) stop ("Object cwm must be of 'cwm' class")
  env <- as.data.frame (env)
  # if ((method == 'lm') & ('max' %in% test || 'colbased' %in% test || 'modified' %in% test) & (dependence == 'cwm ~ env') & (ncol (env) > 1)) stop ("Column-based (modified) and max permutation test is not available for multiple linear regression with more than one predictor variable (env)")  # This can be fixed and removed
  com <- attr (cwm, 'com')
  traits <- attr (cwm, 'traits')

# correlation - function 'cor'
  if (method == 'cor')
  {
    vars.names <- expand.grid (traits = colnames (traits), env = colnames (env), stringsAsFactors = FALSE)

    if (is.null (parallel)) {
      res <- apply (vars.names, 1, FUN = function (var12) test_cwm_cor (e = env[, var12['env'], drop = TRUE], L = as.matrix (com), t = traits[, var12['traits'], drop = TRUE], test = test, perm = perm, wcor = wcor, wstand = wstand))
    }
       else 
      {
        cl <- parallel::makeCluster (parallel)
        parallel::clusterExport (cl, varlist = c('vars.names', 'com', 'traits', 'env', 'test', 'perm'), envir = environment ())
        parallel::clusterEvalQ (cl, eval (call ('library', 'weimea')))
        res <- parallel::parApply (cl, vars.names, 1, FUN = function (var12) test_cwm_cor (e = as.matrix (env[, var12['env'], drop = F]), L = as.matrix (com), t = as.matrix (traits[, var12['traits'], drop = F]), test = test, perm = perm, wcor = wcor, wstand = wstand))
      }
    names (res) <- apply (vars.names, 1, FUN = function (x) paste (x[1], x[2], sep = ' / '))
   }

# linear regression - function 'lm'
  # if (method == 'lm')
  # {
  #   vars.names <- if (dependence == 'cwm ~ env') colnames (traits) else colnames (env)
  #   
  #   if (is.null (parallel)) {
  #     if (dependence == 'cwm ~ env') res <- lapply (vars.names, FUN = function (var12) weimea:::test_cwm_lm (e = as.matrix (env), L = as.matrix (com), t = as.matrix (traits[, var12, drop = FALSE]), test = test, dependence = dependence, perm = perm, wstand = wstand)) else
  #       res <- lapply (vars.names, FUN = function (var12) test_cwm_lm (e = as.matrix (env[, var12, drop = FALSE]), L = as.matrix (com), t = as.matrix (traits), test = test, dependence = dependence, perm = perm, wstand = wstand))
  #   }
  #   else 
  #   {
  #     cl <- parallel::makeCluster (parallel)
  #     parallel::clusterExport (cl, varlist = c('vars.names', 'com', 'traits', 'env', 'test', 'perm'), envir = environment ())
  #     parallel::clusterEvalQ (cl, eval (call ('library', 'weimea')))
  #     if (dependence == 'cwm ~ env') res <- parallel::parApply (cl, vars.names, 1, FUN = function (var12) test_cwm_lm (e = as.matrix (env), L = as.matrix (com), t = as.matrix (traits[, var12, drop = FALSE]), test = test, dependence = dependence, perm = perm, wstand = wstand)) else
  #       res <- parallel::parApply (cl, vars.names, 1, FUN = function (var12) test_cwm_lm (e = as.matrix (env[, var12, drop = FALSE]), L = as.matrix (com), t = as.matrix (traits), test = test, dependence = dependence, perm = perm, wstand = wstand))
  #   }
  #   names (res) <- vars.names
  #   lapply (res, FUN = function (RES) {
  #     R.temp <- as.list (as.vector (cbind (RES$coef, RES$stderr)))
  #     THIS NEEDS TO FINISH
  #   })
  # }
  
#     # linear regression - function fastLM_cwm
#   if (method == 'lm')
#   {
#     vars.names <- if (dependence == 'cwm ~ env') colnames (cwm) else colnames (env)
#       
#     if (is.null (parallel)) {
#       if (dependence == 'cwm ~ env') 
#         res <- lapply (vars.names, FUN = function (var1) test_cwm_lm (com = as.matrix (com), traits = as.matrix (traits[, var1, drop = F]), env = as.matrix (env), test = test, dependence = dependence, perm = perm)) else
#           res <- lapply (vars.names, FUN = function (var1) test_cwm_lm (com = as.matrix (com), traits = as.matrix (traits), env = as.matrix (env[, var1, drop = F]), test = test, dependence = dependence, perm = perm))
#     } else
#        {
#         cl <- parallel::makeCluster (parallel)
#         parallel::clusterExport (cl, varlist = c('vars.names', 'com', 'traits', 'env', 'test', 'dependence', 'perm', 'testLR.P', 'testLR.perm'), envir = environment ())
#         parallel::clusterEvalQ (cl, eval (call ('library', 'weimea')))
#         if (dependence == 'cwm ~ env') 
#           res <- parallel::parLapply (cl, vars.names, FUN = function (var1) test_cwm_lm (com = as.matrix (com), traits = as.matrix (traits[, var1, drop = F]), env = as.matrix (env), test = test, dependence = dependence, perm = perm)) else
#             res <- parallel::parLapply (cl, vars.names, FUN = function (var1) test_cwm_lm (com = as.matrix (com), traits = as.matrix (traits), env = as.matrix (env[, var1, drop = F]), test = test, dependence = dependence, perm = perm))
#       }
#   lapply (res, FUN = function (x) x$coef)  
#   names (res) <- vars.names
#   for (i in seq (1, length (res))) rownames (res[[i]]$coef) <- c ('intercept', names (env))
#   }
# 
#   if (method == 'fourthcorner')   # implementation of fourth.corner.ade doesn't allow to go parallel
#   {
#     res <- fourth.corner (L = com, Q = traits, R = env, chessel = chessel)
#   }
  res.coef <- lapply (res, FUN = function (x) x[1:(length (x)-3)])
  res.miss <- lapply (res, FUN = function (x) x[(length (x)-2):length (x)])
  res.out <- do.call (rbind.data.frame, res.coef)
  test.choice <- c('parametric', 'standard', 'rowbased', 'modified', 'colbased', 'max')
  test.P.xxx <- c ('P_par', 'P_row', 'P_row', 'P_col', 'P_col', 'P_max')
  P.test <- test.P.xxx[test.choice %in% test]
  res.coefs <- res.out [,!substr (colnames (res.out), 1, 2) %in% 'P_']
  res.out <- cbind (res.coefs, res.out[, P.test, drop = F])
  if (adjustP && test != 'none') {
    res.out <- cbind (res.out, p.adjust (res.out[, ncol (res.out)], method = p.adjust.method))
    colnames (res.out)[ncol (res.out)] <- paste (colnames (res.out)[ncol (res.out)-1], 'adj', sep = '_')
  }
  
  result <- list (call = CALL, out = res.out, miss = res.miss, param = list (method = method, wcor = wcor, wstand = wstand, dependence = dependence, perm = perm, test = test, P.test = P.test, adjustP = adjustP, p.adjust.method = p.adjust.method), var.names = list (env = colnames (env), traits = colnames (traits)), orig.data = list (cwm = cwm, env = env))
  class (result) <- 'testCWM'
  return (result)
}

#' @rdname test_cwm
#' @export
print.testCWM <- function (x, digits = max(3, getOption("digits") - 3), missing.summary = FALSE, adjustP = FALSE, ...)
{
  cs.ind <- switch (x$param$method, 
                    'cor' = 1,
                    'fourthcorner' = 1:2)
  tst.ind <- switch (x$param$method,
                     'cor' = 2,
                     'fourthcorner' = 0)
  df.ind <- switch (x$param$method,
                    'cor' = 3:4,
                    'fourthcorner' = 3:4)
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  printCoefmat2 (x$out, digits = digits, cs.ind = cs.ind, tst.ind = tst.ind, df.ind = df.ind, ...)
  if (missing.summary) printMissum (x$miss)
  
}

printMissum <- function (x){
  miss <- lapply (x, FUN = function (a) lapply (a, length))
  miss <- as.matrix (do.call (rbind.data.frame, miss))
  colnames (miss) <- c('env', 'traits', 'samples')
  if (sum (miss) == 0) cat("\nThere are no missing env, traits or sample values.\n") else
  {
    cat("\nCount of missing values:\n")
    print.default (miss)
  }
  invisible (miss)
  }

#' @rdname test_cwm
#' @export
coef.testCWM <- function (object, ...)
  return (object$out)
 
test.cwm.cor.spea <- function (com, traits, env, test, perm, testLR_P, testLR_perm) 
{
  com_temp <- com [!is.na (env), !is.na(traits)]
  traits_temp <- traits [!is.na (traits)]
  env_temp <- env [!is.na (env)]
  CWM <- cwm (com_temp, traits_temp)
  no_samples <- nrow (com_temp)
  cor_temp <- cor.test (CWM, env_temp, method = "spearman")  # calculates Spearman's rho correlation coefficient
  rho_obs <- cor_temp$estimate  
  S_obs <- cor_temp$statistic  #(no_samples^3 - no_samples) * (1 - rho_obs)/6   # calculates S value
  P_par <- cor_temp$p.value
  P_sta <- NA
  P_mod <- NA
  P_max <- NA
  env_rand <- NA
  
  if (test %in% c("standard", "rowbased", "twostep")){
    rho_exp_sta <- vector ('numeric', length = perm + 1)
    S_exp_sta <- vector ('numeric', length = perm + 1)
    for (nperm in seq (1, perm)){
      env_rand <- sample (env_temp) 
      rho_exp_sta [nperm] <- as.vector (cor (CWM, env_rand, method = 'spearman'))  # original cwm with randomized R; there must be 'as.vector', otherwise the output is matrix and produces problem later in logical comparisons
      S_exp_sta [nperm] <- (no_samples^3 - no_samples) * (1 - rho_exp_sta [nperm])/6
    }
    rho_exp_sta [perm+1] <- rho_obs #observed values is put on the last place of the vector
    S_exp_sta [perm+1] <- S_obs #observed values is put on the last place of the vector
    P_sta = min (sum (S_exp_sta >= S_obs), sum (S_exp_sta <= S_obs))/(perm + 1)
  }
  
  if (test %in% c("modified", "twostep")){
    rho_exp_mod <- vector ('numeric', length = perm + 1)
    S_exp_mod <- vector ('numeric', length = perm + 1)
    for (nperm in seq (1, perm)){
      cwm_rand <- cwm (com_temp, sample (traits_temp))
      rho_exp_mod [nperm] <- as.vector (cor (cwm_rand, env_temp, method = 'spearman')) # original R with cwm from randomized traits
      S_exp_mod [nperm] <- (no_samples^3 - no_samples) * (1 - rho_exp_mod [nperm])/6
    }
    rho_exp_mod [perm + 1] <- rho_obs # observed values is put on the last place of the vector
    S_exp_mod [perm + 1] <- S_obs  # observed values is put on the last place of the vector
    P_mod <- min (sum (S_exp_mod >= S_obs), sum (S_exp_mod <= S_obs))/(perm + 1)
  };
  
  if (test %in% "twostep"){
    cwm_art <- cwm (com_temp, cwm (t(com_temp), env_temp))
    rho_obs_LR <- as.vector (cor (cwm_art, env_temp, method = 'spearman'))  # calculates Spearman's rho correlation coefficient
    S_obs_LR <- (no_samples^3 - no_samples) * (1 - rho_obs_LR)/6
    rho_exp_LR <- vector ('numeric', length = testLR_perm + 1)
    S_exp_LR <- vector ('numeric', length = testLR_perm + 1)
    for (nperm in seq (1, testLR_perm)){
      env_rand <- sample (env_temp)
      cwm_exp <- cwm (com_temp, cwm (t(com_temp), env_rand))
      rho_exp_LR [nperm] <- as.vector (cor (cwm_exp, env_rand, method = 'spearman'))
      S_exp_LR [nperm] <- (no_samples^3 - no_samples) * (1 - rho_exp_LR[nperm])/6
    }
    rho_exp_LR [testLR_perm + 1] <- rho_obs_LR # observed values is put on the last place of the vector
    S_exp_LR [testLR_perm + 1] <- S_obs_LR # observed values is put on the last place of the vector
    P_LR <- min (sum (S_exp_LR >= S_obs_LR), sum (S_exp_LR <= S_obs_LR))/(perm + 1)
    if (P_LR <= testLR_P) {P_two = P_mod} else {P_two = P_sta}
  }
  
  return (list (
    rho.obs = rho_obs,
    S.obs = S_obs,
    P.par = P_par,
    P.row = P_sta,
    P.col = P_mod,
    P.LR = P_LR,
    P.two = P_two
  ))
}

join_mat <- function (mat1, mat2, incl1names = TRUE, incl2names = TRUE) {
  mat12 <- lapply (1: min (ncol (mat1), ncol (mat2)), FUN = function (i) {
    mat12_temp <- cbind (mat1[,i], mat2[,i])
    if (incl1names || incl2names) colnames (mat12_temp) <- c (if (incl1names) colnames (mat1)[i] else "", if (incl2names) colnames (mat2)[i] else "")
    mat12_temp})
  mat12 <- as.matrix (data.frame (mat12, check.names = FALSE, fix.empty.names = TRUE))
  colnames (mat12)[seq (2, ncol (mat12), by = 2)] <- ''  # this makes sure that P.values *** will not have colnames (a bit stupid solution here)
  return (mat12)
}

printCoefmat2 <- function (x, digits = max(3L, getOption("digits") - 2L), signif.stars = getOption("show.signif.stars"), 
                           signif.legend = signif.stars, dig.tst = max(1L, min(5L, digits - 1L)), cs.ind = NULL, tst.ind = NULL, df.ind = NULL, zap.ind = integer(), eps.Pvalue = .Machine$double.eps, na.print = "NA", ...) 
{
  d <- dim(x)
  if (is.null(d) || length(d) != 2L) stop("'x' must be coefficient matrix/data frame")
  nc <- d[2L]
  xm <- data.matrix(x)
  Cf <- array("", dim = c(d[1], sum (cs.ind > 0) + sum (tst.ind > 0) + sum (df.ind > 0)), dimnames = list (dimnames(xm)[[1]], dimnames (xm)[[2]][1:(sum (cs.ind > 0) + sum (tst.ind > 0) + sum (df.ind > 0))]))
  ok <- !(ina <- is.na(xm))
  for (i in zap.ind) xm[, i] <- zapsmall(xm[, i], digits)
  if (length(cs.ind)) {
    acs <- abs(coef.se <- xm[, cs.ind, drop = FALSE])
    if (any(ia <- is.finite(acs))) {
      digmin <- 1 + if (length(acs <- acs[ia & acs != 
                                          0])) 
        floor(log10(range(acs[acs != 0], finite = TRUE)))
      else 0
      Cf[, cs.ind] <- format(round(coef.se, max(1L, digits - 
                                                  digmin)), digits = digits)
    }
  }
  if (length(tst.ind)) 
    Cf[, tst.ind] <- format(round(xm[, tst.ind], digits = dig.tst), 
                            digits = digits)
  ok[, tst.ind] <- FALSE
  if (length(df.ind))
    Cf[, df.ind] <- format (xm[, df.ind])
  
  dec <- getOption("OutDec")
  if (dec != ".") 
    x1 <- chartr(dec, ".", x1)
  if (any(ina)) 
    Cf[ina] <- na.print
  P.values.log <- substr (colnames (xm), 1, 2) == 'P_'
  if (any (P.values.log)) P.values <- colnames (xm) [P.values.log] else P.values <- NULL
  if (!is.null (P.values))
   {
    if (!is.logical(signif.stars) || is.na(signif.stars)) {
      warning("option \"show.signif.stars\" is invalid: assuming TRUE")
      signif.stars <- TRUE
    }
    pv <- xm[, P.values, drop = F]
    pv_f <- pv
    for (co in ncol (pv_f)) pv_f[,co] <- format.pval(pv[,co], digits = dig.tst, eps = eps.Pvalue)
    #dimnames (pv_f) <- dimnames (pv)
    signif.stars <- signif.stars && any (pv < 0.1)
    if (signif.stars) {
      Signif <- symnum(as.matrix (pv), corr = FALSE, na = FALSE, 
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                       symbols = c("***", "**", "*", ".", " "))
      Cf <- cbind(Cf, join_mat (pv_f, format (Signif), incl2names = FALSE))
    } else Cf <- cbind (Cf, pv_f)
  }
  else signif.stars <- FALSE
  
  Cf <- cbind (`Trait / env` = rownames (Cf), Cf)
  rownames (Cf) <- 1:nrow (Cf)
  
  print.default(Cf, quote = FALSE, right = TRUE, na.print = na.print, 
                ...)
  if (signif.stars && signif.legend) {
    if ((w <- getOption("width")) < nchar(sleg <- attr(Signif, 
                                                       "legend"))) 
      sleg <- strwrap(sleg, width = w - 2, prefix = "  ")
    cat("---\nSignif. codes:  ", sleg, sep = "", fill = w + 
          4 + max(nchar(sleg, "bytes") - nchar(sleg)))
  }
  invisible(x)
}

#' @rdname test_cwm
#' @export
plot.testCWM <- function (x, alpha = 0.05, line = 2.5, cex.lab = 1.5, par.mar = c(4,4,0.5, 0.5), box.col = c('blue', 'red'), box.lwd = 2, ...){
  
  trait.names <- x$var.names$traits
  env.names <- x$var.names$env
  CWM <- x$orig.data$cwm
  ENV <- x$orig.data$env
  
  par (mfcol = c(length (trait.names), length (env.names)))
  par (mar = par.mar)
  
  test <- x$out[max (which (substr (colnames (x$out), 1, 2) %in% 'P_'))]
  
  traits.env <- expand.grid (traits = trait.names, env = env.names)
  if (!is.null (test)) traits.env <- cbind (traits.env, test = test, r = x$out$r)
  
  apply (traits.env, 1, FUN = function (te){
    plot (CWM[[te[1]]] ~ ENV[,te[2]], pch = 16, ann = F)
    title (xlab = te[2], ylab = te[1], line = line, cex.lab = cex.lab)
    if (!is.null (test)) {
      bc <- ifelse (as.numeric (te[4]) < 0, box.col[1], box.col[2])
      if (as.numeric (te[3]) <= alpha) box (col = bc, lwd = box.lwd)
    }
  })
}
