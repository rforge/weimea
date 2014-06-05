
simple.data.check <- function (calc.analysis,  sp.data)
{
  if (calc.analysis == "ANOVA") if (!all (rownames (groups) == rownames (veg))) if (tclvalue (tkmessageBox (type = "yesno", message = "Rownames of Vegetation matrix and Groups of samples have different names. Proceed anyway?")) == "no") break
  if (!calc.analysis == "ANOVA") if (!all (rownames (external) == rownames (veg))) if (tclvalue (tkmessageBox (type = "yesno", message = "Rownames of Vegetation matrix and External variable(s) have different names. Proceed anyway?")) == "no") break
  if (sp.data == "EIVs")
    if (!all (rownames (spec.iv) == colnames (veg))) 
      if (tclvalue (tkmessageBox (type = "yesno", message = "Species names in Vegetation matrix and Species indicator values are different. Proceed anyway?")) == "no") break else
        if (tclvalue (tkmessageBox (type = "yesno", message = "Do you want to see the list of wrong species names?")) == "yes") 
        {
          write.table (cbind (SpeciesData = rownames (spec.iv), Table = colnames (veg))[rownames (spec.iv) != colnames (veg),], file = 'species-names-missmatch.txt')
          try(shell.exec (paste (getwd(), "/species-names-missmatch.txt", sep = "")))
        }
  if (sp.data == "EIVs") if (!all (apply (spec.iv, 2, "%in%", c(1:12, NA))))  if (tclvalue (tkmessageBox (type = "yesno", message = "Table with species indicator values contains non-numeric values. Should an attempt to clean them automatically be done?")) == "no") break else {spec.iv.temp <- clean.ell (spec.iv); if (!all (apply (spec.iv.temp, 2, "%in%", c(1:12, NA)))) tkmessageBox (type = "ok", message = "Table with species indicator values cannot be cleaned automatically, please proceed manualy.") else assign ("spec.iv", spec.iv.temp, envir = .GlobalEnv)}
  if (sp.data == "sp.data") if (!all (rownames (spec.data) == colnames (veg))) if (tclvalue (tkmessageBox (type = "yesno", message = "Species names in Vegetation matrix and Species indicator values are different. Proceed anyway?")) == "no") break
}

#pval <- function (p) ifelse (p < 0.001, '< 0.001', ifelse (p < 0.01, '< 0.01', ifelse (p < 0.05, '< 0.05', paste ('= ', formatC (p, digits = 3, format = 'f'), sep = ''))))

# real.iv <- function (veg, spec.iv, FUN = function (x) {mean (x, na.rm = T)})
# {
#   spec.iv <- as.matrix (spec.iv)
#   veg[veg > 0] <- 1
#   veg[veg == 0] <- NA
#   spec.iv <- as.matrix (spec.iv)
#   temp.result<- apply (apply (spec.iv, 2, FUN = function (x) colMeans (t(veg) * x, na.rm = T)), 2, FUN)
#   temp.result
# }


# randomize.iv <- function (veg, spec.iv, permutations = 499, FUN = function (x) {mean (x, na.rm = T)}, progress.bar = T)
# {
#   if (progress.bar) win.pb <- winProgressBar(title = "Permutation progress bar", label = "", min = 0, max = permutations, initial = 0, width = 300)
#   spec.iv <- as.matrix (spec.iv)
#   #veg[veg > 0] <- 1
#   #veg[veg == 0] <- NA
#   temp.result <- list ()
#   for (perm in seq (1, permutations))
#   {
#     setWinProgressBar (win.pb, perm)
#     temp.result[[perm]] <- apply (apply (spec.iv, 2, FUN = function (x) colMeans (t(veg) * sample (x), na.rm = T)), 2, FUN)
#   }
#   close (win.pb)
#   temp.result
# }

# randomize.iv.mean <- function (veg, spec.iv, permutations = 499, FUN = function (x) {mean (x, na.rm = T)}, progress.bar = T)
# {
#   if (progress.bar) win.pb <- winProgressBar(title = "Permutation progress bar", label = "", min = 0, max = permutations, initial = 0, width = 300)
#   spec.iv <- as.matrix (spec.iv)
#   #veg[veg > 0] <- 1
#   #veg[veg == 0] <- NA
#   temp.result <- list ()
#   for (perm in seq (1, permutations))
#   {
#     setWinProgressBar (win.pb, perm)
#     temp.result[[perm]] <- apply (apply (spec.iv, 2, FUN = function (x) sample (colMeans (t(veg) * x, na.rm = T))), 2, FUN)
#   }
#   close (win.pb)
#   temp.result
# }


# summary.aov.iv <- function (x, veg, spec.iv, permutations = 499)
# {
#   pb <- winProgressBar(title = "Permutation progress bar", label = "", min = 0, max = 100, initial = 0, width = 300)
#   pb.add <- 100/(permutations * ncol (spec.iv))
#   pbtemp <- 0
#   x <- as.factor (x)
#   spec.iv <- as.matrix (spec.iv)
#   veg [veg == 0] <- NA
#   
#   apply.FUN <- function (x) 
#   {
#     colMeans (t(veg)*x, na.rm = T)
#   }
#   
#   apply.FUN.sample <- function (x) 
#   {
#     veg.temp <- veg[, !is.na (x)]
#     x.temp <- x[!is.na (x)]
#     colMeans (t(veg.temp)*sample (x.temp), na.rm = T)
#   }
#   
#   FUN.F <- function (x) x[1,4]
#   
#   mean.iv <- apply (spec.iv, 2, FUN = apply.FUN)
#   ss <- summary (aov (mean.iv ~ x))
#   real.F <- unlist (lapply (ss, FUN.F))  
#   
#   rand.F <- matrix (nrow = permutations, ncol = dim (spec.iv)[2])
#   pb.temp <- 0
#   for (i in seq (1, ncol (spec.iv)))
#   {
#     spec.iv.temp <- spec.iv[,i]
#     veg.temp <- veg[, !is.na (spec.iv.temp)]
#     spec.iv.temp <- spec.iv.temp [!is.na (spec.iv.temp)]
#     for (r in seq (1, permutations))
#     {
#       mean.iv.rand <- colMeans (t(veg.temp)*sample (spec.iv.temp), na.rm = T)
#       rand.F[r,i] <- unlist (lapply (summary (aov (mean.iv.rand ~ x)), FUN.F))
#       pb.temp <- pb.temp + pb.add
#       setWinProgressBar (pb, pb.temp)
#     }
#   }
#   permstore <- sweep (rand.F, 2, real.F, '>')  
#   rand.pvals <- (apply(permstore, 2, sum) + 1)/(permutations + 1)
#   for (j in seq (dim (mean.iv)[2])) ss[[j]][1,5] <- rand.pvals[j]
#   close (pb)
#   ss
# }

clean.ell <- function (data = JUICE.species.data)
{
  ell.names <- c("Ellenberg_Light", "Ellenberg_Temperature", "Ellenberg_Continentality", "Ellenberg_Moisture", "Ellenberg_Soil_Reaction", "Ellenberg_Nutrients")
  temp.ell <- as.matrix (data[, ell.names])
  #temp.ell <- as.matrix (data)
  to.delete <- c ("", "-", "0?", "0x", "1?", "1x", "2?", "2x", "x", "0")
  temp.ell[as.matrix (apply (temp.ell, 2, FUN = function (x) {x %in% to.delete}))] <- NA
  suppressWarnings (class (temp.ell) <- "numeric")
  #if (all (is.na (temp.ell))) if (tclvalue (tkmessageBox (type = "yesno", message = "There are no Ellenberg indicator values. Perhaps you forgot to initiate them in JUICE. Proceed anyway?")) == "no") break
  data [, ell.names] <- temp.ell
  as.data.frame (temp.ell)
}


calc.anova <- function (permutations, species.data)
{
  # preparation of data - same for all functions
  veg <- veg > 0
  class (veg) <- 'numeric'
  veg [!veg] <- NA
  if (species.data == 'EIVs') spec.iv <- spec.iv else spec.iv <- spec.data
  real.mean.eiv <- real.iv (veg = veg, spec.iv = spec.iv, FUN = function (x) x)
  
  sum.par <- summary (aov (real.mean.eiv ~ as.factor (groups[,1])))
  real.F <- matrix (unlist (sum.par), ncol = 10, byrow = T)[,7]
  if (as.numeric (tclvalue (P.perm.test)))
  {
    sum.perm <- matrix (unlist (randomize.iv.mean (veg = veg, spec.iv = spec.iv, permutations = permutations, FUN = function (x) summary (aov (x ~ groups[,1]))[[1]][["F value"]][1])), ncol = ncol (spec.iv), byrow = T)
    sum.perm <- (colSums (sweep (sum.perm, 2, real.F, '>'))+1)/(permutations + 1)
  }
  if (as.numeric (tclvalue (P.modif.test)))
  {
    sum.modif <- matrix (unlist (randomize.iv (veg = veg, spec.iv = spec.iv, permutations = permutations, FUN = function (x) summary (aov (x ~ groups[,1]))[[1]][["F value"]][1])), ncol = ncol (spec.iv), byrow = T)
    sum.modif <- (colSums (sweep (sum.modif, 2, real.F, '>'))+1)/(permutations + 1)
  }
  
  result.par <- matrix (unlist (sum.par), nrow = ncol (spec.iv), byrow = T, dimnames = list (names (spec.iv), c('Df', 'ResDf', 'SumSq', 'ResSumSq', 'MeanSq', 'ResMeanSq', 'F-val', 'X', 'P.orig', 'XX')))[,c(-8, -10), drop = F]
  iv.result.table <- result.par [,-8, drop = F]
  if (as.numeric (tclvalue (P.par.test))) iv.result.table <- cbind (iv.result.table, P.par = result.par[,8])
  if (as.numeric (tclvalue (P.perm.test))) iv.result.table <- cbind (iv.result.table, P.perm = sum.perm)
  if (as.numeric (tclvalue (P.modif.test))) iv.result.table <- cbind (iv.result.table, P.modif = sum.modif)
  write.table (file = 'iv_result_table.txt', paste ('ANOVA of mean indicator (or species) values between groups', sep = ''), row.names = F, col.names = F, append = F)
  write.table (file = 'iv_result_table.txt', iv.result.table, sep = '\t', row.names = T, col.names = NA, quote = T, qmethod = 'double', append = T)
  
  
  if (dev.cur () == 1) windows (record = T)
  if (species.data == 'EIVs') par (mfrow = c (3, 2)) else par (mfrow = c (1,1))
  
  for (i in seq (1, ncol (spec.iv))) 
  {
    main.temp <- paste ('F = ', formatC (sum.par[[i]][1,4], digits = 3, format = 'f'), sep = '')
    if (as.numeric (tclvalue (P.par.test))) main.temp <- paste (main.temp, ', P.par ' , pval (sum.par [[i]][1,5]), sep = '')
    if (as.numeric (tclvalue (P.perm.test))) main.temp <- paste (main.temp, ', P.perm ', pval (sum.perm [i]), sep = '')
    if (as.numeric (tclvalue (P.modif.test))) main.temp <- paste (main.temp, ', P.modif ', pval (sum.modif [i]), sep = '')
    boxplot (real.mean.eiv[,i] ~ as.factor (groups[,1]), ylab = colnames (real.mean.eiv)[i], main = list (main.temp, cex = 1.0))
  }
  
  bringToTop (dev.cur(), stay = F)
}

calc.reg <- function (permutations, species.data)
{
  # preparation of data - same for all functions
  veg <- veg > 0
  class (veg) <- 'numeric'
  veg [!veg] <- NA
  if (species.data == 'EIVs') spec.iv <- spec.iv else spec.iv <- spec.data
  real.mean.eiv <- real.iv (veg = veg, spec.iv = spec.iv, FUN = function (x) x)
  
  P.param <- matrix (ncol = ncol (spec.iv), nrow = ncol (external))
  P.perm <- matrix (ncol = ncol (spec.iv), nrow = ncol (external))
  P.modif <- matrix (ncol = ncol (spec.iv), nrow = ncol (external))
  summary.param <- list ()
  
  for (i in seq (1, ncol (external)))
  {
    summary.param [[i]]<- apply (real.mean.eiv, 2, FUN = function (x) {temp <- if (tclvalue (reg.direction) == 'E-IV') summary(lm (external[,i] ~ x)) else summary (lm (x ~ external[,i])) })
    temp.param <- unlist (lapply (summary.param[[i]], FUN = function (x) x$fstatistic[1]))
    P.param[i,] <- unlist (lapply (summary.param[[i]], FUN = function (x) coef (x)[2,4]))
    
    if (as.numeric (tclvalue (P.perm.test)))    
    {
      temp.perm <- randomize.iv.mean (veg, spec.iv, permutations, FUN = function (x) if (tclvalue (reg.direction) == 'E-IV') summary (lm (external[,i] ~ x))$fstatistic[1] else summary (lm (x ~ external[,i]))$fstatistic[1])
      temp.perm <- matrix (unlist (temp.perm), nrow = permutations, byrow = T)
      P.perm[i,] <- (colSums (sweep (temp.perm, 2, temp.param, '>'))+1)/(permutations+1)
    }
    
    if (as.numeric (tclvalue (P.modif.test)))
    {
      temp.modif <- randomize.iv (veg, spec.iv, permutations, FUN = function (x) if (tclvalue (reg.direction) == 'E-IV') summary (lm (external[,i] ~ x))$fstatistic[1] else summary (lm (x ~ external[,i]))$fstatistic[1])
      temp.modif <- matrix (unlist (temp.modif), nrow = permutations, byrow = T)
      P.modif[i,] <- (colSums (sweep (temp.modif, 2, temp.param, '>'))+1)/(permutations+1)
    }
  }
  
  
  for (i in seq (1, ncol (external)))
  {
    result.par <- matrix (unlist (lapply (summary.param[[i]], FUN = function (x) as.vector (t(coef (x))))), ncol = 8, byrow = T, dimnames = list (NULL, c('Intercept Estimate', 'Intercept Std. Error', 'Intercept t value', 'Intercept P value', 'External Estimate', 'External Std. Error', 'External t value', 'External P value')))
    iv.result.table <- cbind (result.par, matrix (unlist (lapply (summary.param[[i]], FUN = function (x) x$fstatistic)), ncol = 3, byrow = T, dimnames = list (NULL, c('F value', 'DF1', 'DF2'))))
    row.names (iv.result.table) <- names (spec.iv)
    if (as.numeric (tclvalue (P.par.test))) iv.result.table <- cbind (iv.result.table, P.par = P.param[i,])
    if (as.numeric (tclvalue (P.perm.test))) iv.result.table <- cbind (iv.result.table, P.perm = P.perm[i,])
    if (as.numeric (tclvalue (P.modif.test))) iv.result.table <- cbind (iv.result.table, P.modif = P.modif[i,])
    if (i > 1) write.table (file = 'iv_result_table.txt', '\n', row.names = F, col.names = F, append = T)
    write.table (file = 'iv_result_table.txt', if (tclvalue (reg.direction) == 'E-IV') paste ('Linear regression of ', names (external)[i], ' on mean indicator (or species) values', sep = '') else paste ('Linear regression of mean indicator (or species) values on ', names (external)[i]), row.names = F, col.names = F, append = if (i == 1) F else T)
    write.table (file = 'iv_result_table.txt', as.data.frame (iv.result.table), append = T, sep = '\t', row.names = T, col.names = NA, qmethod = 'd', quote = T)
  }
  
  if (dev.cur () == 1) windows (record = T)
  if (species.data == 'EIVs') par (mfrow = c(3,2)) else par (mfrow = c(1,1))
  for (j in seq (1, ncol (external)))
  {
    for (i in seq (1, ncol (spec.iv)))
    {
      main.temp <- paste('R2 = ', formatC((summary.param[[j]][[i]])$r.squared, digits = 3, format = 'f'), sep = '')
      if (as.numeric (tclvalue (P.par.test))) main.temp <- paste (main.temp, ', P.par ', pval (P.param[j,i]), sep = '')
      if (as.numeric (tclvalue (P.perm.test))) main.temp <- paste (main.temp, ', P.perm ', pval (P.perm[j,i]), sep = '')
      if (as.numeric (tclvalue (P.modif.test))) main.temp <- paste (main.temp, ', P.modif ', pval (P.modif[j,i]), sep = '')    
      if (tclvalue (reg.direction) == 'E-IV') plot (external[,j] ~ real.mean.eiv[,i], xlab = colnames (real.mean.eiv)[i], ylab = names (external)[j], main =  list (main.temp, cex = 1.0))  else plot (real.mean.eiv[,i] ~ external[,j], ylab = colnames (real.mean.eiv)[i], xlab = names (external)[j], main = list (main.temp, cex = 1.0))
      if (tclvalue (reg.direction) == 'E-IV') abline (lm (external[,j] ~ real.mean.eiv[,i])) else abline (lm (real.mean.eiv[,i] ~ external[,j]))
    }
  }
}

calc.mult.reg <- function (permutations, species.data)
{
  # preparation of data - same for all functions
  veg <- veg > 0
  class (veg) <- 'numeric'
  veg [!veg] <- NA
  if (species.data == 'EIVs') spec.iv <- spec.iv else spec.iv <- spec.data
  real.mean.eiv <- real.iv (veg = veg, spec.iv = spec.iv, FUN = function (x) x)
  
  summary.param <- list ()
  summary.param <- apply (real.mean.eiv, 2, FUN = function (x) {temp <- summary(lm (x ~ as.matrix (external))) })
  R2 <- unlist (lapply (summary.param, FUN = function (x) x$r.squared))
  R2.adj <- unlist (lapply (summary.param, FUN = function (x) x$adj.r.squared))
  P.param <- unlist (lapply (summary.param, FUN = function (x) pf (x$fstatistic[1L], x$fstatistic[2L], x$fstatistic[3L], lower.tail = F)))
  
  
  if (as.numeric (tclvalue (P.perm.test)))
  {
    temp.perm <- randomize.iv.mean (veg, spec.iv, permutations, FUN = function (x) summary (lm (x ~ as.matrix (external)))$r.squared)
    temp.perm <- matrix (unlist (temp.perm), nrow = permutations, byrow = T)
    P.perm <- (colSums (sweep (temp.perm, 2, R2, '>'))+1)/(permutations+1)
  }
  
  if (as.numeric (tclvalue (P.modif.test)))
  {
    temp.modif <- randomize.iv (veg, spec.iv, permutations, FUN = function (x) summary (lm (x ~ as.matrix (external)))$r.squared)
    temp.modif <- matrix (unlist (temp.modif), nrow = permutations, byrow = T)
    P.modif <- (colSums (sweep (temp.modif, 2, R2, '>'))+1)/(permutations+1)
    R2.adj.iv <- 1 - ((1-R2)/(1-colMeans (temp.modif)))
  }
  
  result.par <- matrix (unlist (lapply( summary.param, FUN = function (x) x$coef[-1,1])), ncol = ncol (external) , byrow = T, dimnames = list (names (spec.iv), names (external)))
  iv.result.table <- cbind (result.par, R2 = R2, R2.adj = R2.adj)
  if (as.numeric (tclvalue (P.modif.test))) iv.result.table <- cbind (iv.result.table, R2.adj.iv = R2.adj.iv)
  
  if (as.numeric (tclvalue (P.par.test))) iv.result.table <- cbind (iv.result.table, P.par = P.param)
  if (as.numeric (tclvalue (P.perm.test))) iv.result.table <- cbind (iv.result.table, P.perm = as.vector (P.perm))
  if (as.numeric (tclvalue (P.modif.test))) iv.result.table <- cbind (iv.result.table, P.modif = as.vector (P.modif))
  write.table (file = 'iv_result_table.txt', as.data.frame (iv.result.table), append = F, sep = '\t', row.names = T, col.names = NA, qmethod = 'd', quote = T)
  
  require (vegan)
  if (dev.cur () == 1) windows (record = T)
  external.plot <- scale (external, center = T, scale = F)
  ordiplot (scale (external.plot))
  plot (envfit (scale (external.plot), real.mean.eiv, perm = 0, na.rm = T))
}


calc.cor <- function (permutations, species.data)
{
  # preparation of data - same for all functions
  veg <- veg > 0
  class (veg) <- 'numeric'
  veg [!veg] <- NA
  if (species.data == 'EIVs') spec.iv <- spec.iv else spec.iv <- spec.data
  real.mean.eiv <- real.iv (veg = veg, spec.iv = spec.iv, FUN = function (x) x)
  
  P.param <- matrix (ncol = ncol (spec.iv), nrow = ncol (external))
  P.perm <- matrix (ncol = ncol (spec.iv), nrow = ncol (external))
  P.modif <- matrix (ncol = ncol (spec.iv), nrow = ncol (external))
  summary.param <- list ()
  
  for (i in seq (1, ncol (external)))
  {
    summary.param [[i]]<- apply (real.mean.eiv, 2, FUN = function (x) temp <- cor.test (external[,i], x, method = tclvalue (cor.method), use = 'complete.obs'))
    temp.param <- unlist (lapply (summary.param[[i]], FUN = function (x) x$estimate))
    P.param[i,] <- unlist (lapply (summary.param[[i]], FUN = function (x) x$p.value))
    
    if (as.numeric (tclvalue (P.perm.test)))    
    {
      temp.perm <- randomize.iv.mean (veg, spec.iv, permutations, FUN = function (x) cor (external[,i], x, method = tclvalue (cor.method), use = 'complete.obs'))
      temp.perm <- matrix (unlist (temp.perm), nrow = permutations, byrow = T)
      P.perm[i,] <- apply (rbind((2*(colSums (sweep (temp.perm, 2, temp.param, '>'))+1))/(permutations+1), (2*(colSums (sweep (temp.perm, 2, temp.param, '<'))+1))/(permutations+1)), 2, min)
    }
    
    if (as.numeric (tclvalue (P.modif.test)))
    {
      temp.modif <- randomize.iv (veg, spec.iv, permutations, FUN = function (x) cor (external[,i], x, method = tclvalue (cor.method), use = 'complete.obs'))
      temp.modif <- matrix (unlist (temp.modif), nrow = permutations, byrow = T)
      P.modif[i,] <- apply (rbind((2*(colSums (sweep (temp.modif, 2, temp.param, '>'))+1))/(permutations+1), (2*(colSums (sweep (temp.modif, 2, temp.param, '<'))+1))/(permutations+1)), 2, min)
    }
  }
  
  for (i in seq (1, ncol (external)))
  {
    iv.result.table <- matrix (unlist(lapply(summary.param[[i]], FUN = function (x) c(x$estimate, x$statistic, x$parameter))), nrow = ncol (spec.iv), byrow = T, dimnames = list (names (spec.iv), c(paste (names (summary.param[[i]][[1]]$estimate), 'estimate'), paste (names (summary.param[[i]][[1]]$statistic), 'statistic'), 'Df')[if (tclvalue (cor.method)=='pearson') 1:3 else 1:2]))
    if (as.numeric (tclvalue (P.par.test))) iv.result.table <- cbind (iv.result.table, P.par = P.param[i,])
    if (as.numeric (tclvalue (P.perm.test))) iv.result.table <- cbind (iv.result.table, P.perm = P.perm[i,])
    if (as.numeric (tclvalue (P.modif.test))) iv.result.table <- cbind (iv.result.table, P.modif = P.modif[i,])
    if (i > 1) write.table (file = 'iv_result_table.txt', '\n', row.names = F, col.names = F, append = T)
    write.table (file = 'iv_result_table.txt', paste ('Correlation between ', names (external)[i], ' and mean indicator (or species) values' ,  sep = '') , row.names = F, col.names = F, append = if (i == 1) F else T)
    write.table (file = 'iv_result_table.txt', as.data.frame (iv.result.table), append = T, sep = '\t', row.names = T, col.names = NA, qmethod = 'd', quote = T)
  }
  
  if (dev.cur () == 1) windows (record = T)
  if (species.data == 'EIVs') par (mfrow = c(3,2)) else par (mfrow = c(1,1))
  for (j in seq (1, ncol (external)))
  {
    for (i in seq (1, ncol (spec.iv)))
    {
      main.temp <- paste(names (summary.param[[j]][[1]]$estimate), ' = ', formatC((summary.param[[j]][[i]])$estimate, digits = 3, format = 'f'), sep = '')
      if (as.numeric (tclvalue (P.par.test))) main.temp <- paste (main.temp, ', P.par ', pval (P.param[j,i]), sep = '')
      if (as.numeric (tclvalue (P.perm.test))) main.temp <- paste (main.temp, ', P.perm ', pval (P.perm[j,i]), sep = '')
      if (as.numeric (tclvalue (P.modif.test))) main.temp <- paste (main.temp, ', P.modif ', pval (P.modif[j,i]), sep = '')    
      plot (external[,j] ~ real.mean.eiv[,i], xlab = colnames (real.mean.eiv)[i], ylab = names (external)[j], main =  list (main.temp, cex = 1.0))
    }
  }
}


mopet_tcltk <- function ()
{
require (tcltk)
#windows (record = T)
#par (mfrow = c (3, 2), mar = c (2.1, 4.1, 5.1, 1.1))
cancel <- tclVar (0)
randomizations <- tclVar (499)
species.data <- tclVar ('EIVs')
calc.analysis <- tclVar ('ANOVA')
cor.method <- tclVar ('pearson')
reg.direction <- tclVar ('E-IV')
P.par.test <- tclVar (1)
P.perm.test <- tclVar (1)
P.modif.test <- tclVar (1)

base <- tktoplevel ()

tkwm.title(base, "Modified permutation test for weighted mean of species attributes")

super.input.frame <- tkframe (base)
input.frame <- tkframe (super.input.frame, relief = 'groove', borderwidth = 2)
selection.frame <- tkframe (base)
button.frame <- tkframe (base)

input.frame.1 <- tkframe (input.frame)
input.frame.2 <- tkframe (input.frame)
input.frame.3 <- tkframe (input.frame)
input.frame.4 <- tkframe (input.frame)
input.frame.5 <- tkframe (input.frame)

selection.frame.up <- tkframe (selection.frame)
selection.frame.bottom <- tkframe (selection.frame)
selection.frame.1 <- tkframe (selection.frame.up, relief = 'groove', borderwidth = 2)
selection.frame.2 <- tkframe (selection.frame.up, relief = 'groove', borderwidth = 2)
selection.frame.3 <- tkframe (selection.frame.up, relief = 'groove', borderwidth = 2)
selection.frame.4 <- tkframe (selection.frame.bottom, relief = 'groove', borderwidth = 2)
selection.frame.5 <- tkframe (selection.frame.bottom, relief = 'groove', borderwidth = 2)
selection.frame.6 <- tkframe (selection.frame.bottom, relief = 'groove', borderwidth = 2)

button.frame.1 <- tkframe (button.frame)
button.frame.2 <- tkframe (button.frame)

tkpack (tklabel (input.frame, text = 'Data used for analysis:'), anchor = 'w', padx = 5, pady = 5)

open.veg.title <- tklabel (input.frame.1, text = 'Compositional matrix', width = 20, anchor = 'w')
open.veg.lab <- tklabel (input.frame.1, text = '', width = 50, anchor = 'e', relief = 'sunken', background = 'white')
open.veg <- tkbutton (input.frame.1, text = 'Open', width  = 10, command = function () {file.name <- choose.files (filters = Filters[c('txt', 'All'),], index = 1, multi = F); assign ('veg', (read.delim (file.name, head = T, row.names = 1, check.names = F)), envir = .GlobalEnv); tkconfigure (open.veg.lab, text = file.name)})
fix.veg <- tkbutton (input.frame.1, text = 'Edit', width = 10, command = function () {fix (veg)})

open.speciv.title <- tklabel (input.frame.2, text = 'Species attributes (e.g. Ellenberg values or traits)', width = 20, anchor = 'w')
open.speciv.lab <- tklabel (input.frame.2, text = '', width = 50, anchor = 'e', relief = 'sunken', background = 'white')
open.speciv <- tkbutton (input.frame.2, text = 'Open', width  = 10, command = function () {file.name <- choose.files (filters = Filters[c('txt', 'All'),], index = 1, multi = F); assign ('spec.iv', (read.delim (file.name, head = T, row.names = 1, check.names = F)), envir = .GlobalEnv); tkconfigure (open.speciv.lab, text = file.name)})
fix.speciv <- tkbutton (input.frame.2, text = 'Edit', width = 10, command = function () {fix (spec.iv)})

open.specdata.title <- tklabel (input.frame.3, text = 'Species data', width = 20, anchor = 'w')
open.specdata.lab <- tklabel (input.frame.3, text = '', width = 50, anchor = 'e', relief = 'sunken', background = 'white')
open.specdata <- tkbutton (input.frame.3, text = 'Open', width  = 10, command = function () {file.name <- choose.files (filters = Filters[c('txt', 'All'),], index = 1, multi = F); assign ('spec.data', (read.delim (file.name, head = T, row.names = 1, check.names = F)), envir = .GlobalEnv); tkconfigure (open.specdata.lab, text = file.name)})
fix.specdata <- tkbutton (input.frame.3, text = 'Edit', width = 10, command = function () {fix (spec.data)})

open.groups.title <- tklabel (input.frame.4, text = 'Groups of samples', width = 20, anchor = 'w')
open.groups.lab <- tklabel (input.frame.4, text = '', width = 50, anchor = 'e', relief = 'sunken', background = 'white')
open.groups <- tkbutton (input.frame.4, text = 'Open', width  = 10, command = function () {file.name <- choose.files (filters = Filters[c('txt', 'All'),], index = 1, multi = F); assign ('groups', (read.delim (file.name, head = T, row.names = 1, check.names = F)), envir = .GlobalEnv); tkconfigure (open.groups.lab, text = file.name)})
fix.groups <- tkbutton (input.frame.4, text = 'Edit', width = 10, command = function () {fix (groups)})

open.external.title <- tklabel (input.frame.5, text = 'External variable(s)', width = 20, anchor = 'w')
open.external.lab <- tklabel (input.frame.5, text = '', width = 50, anchor = 'e', relief = 'sunken', background = 'white')
open.external <- tkbutton (input.frame.5, text = 'Open', width  = 10, command = function () {file.name <- choose.files (filters = Filters[c('txt', 'All'),], index = 1, multi = F); assign ('external', (read.delim (file.name, head = T, row.names = 1, check.names = F)), envir = .GlobalEnv); tkconfigure (open.external.lab, text = file.name)})
fix.external <- tkbutton (input.frame.5, text = 'Edit', width = 10, command = function () {fix (external)})

tkpack (open.veg.title, open.veg.lab, open.veg, fix.veg, side = 'left', pady = 5, padx = 5)
tkpack (open.speciv.title, open.speciv.lab, open.speciv, fix.speciv, side = 'left', pady = 5, padx = 5)
tkpack (open.specdata.title, open.specdata.lab, open.specdata, fix.specdata, side = 'left', pady = 5, padx = 5)
tkpack (open.groups.title, open.groups.lab, open.groups, fix.groups, side = 'left', pady = 5, padx = 5)
tkpack (open.external.title, open.external.lab, open.external, fix.external, side = 'left', pady = 5, padx = 5)
tkpack (input.frame.1, input.frame.2, input.frame.3, input.frame.4, input.frame.5)
tkpack (input.frame, padx = 5, pady = 5)
tkpack (super.input.frame, fill = 'both', expand = T)

tkpack (tklabel (selection.frame.1, text = 'Calculate:'),
        tkradiobutton (selection.frame.1, text = 'ANOVA between groups', var = calc.analysis, value = 'ANOVA', command = function (...) {tkconfigure (radio.pearson, state = 'disabled'); tkconfigure (radio.kendall, state = 'disabled'); tkconfigure (radio.spearman, state = 'disabled'); tkconfigure (radio.E_IV, state = 'disabled'); tkconfigure (radio.IV_E, state = 'disabled')}),
        tkradiobutton (selection.frame.1, text = 'correlation with external variable(s)', var = calc.analysis, value = 'cor', command = function (...) {tkconfigure (radio.pearson, state = 'active'); tkconfigure (radio.kendall, state = 'active'); tkconfigure (radio.spearman, state = 'active'); tkconfigure (radio.E_IV, state = 'disabled'); tkconfigure (radio.IV_E, state = 'disabled')}),
        tkradiobutton (selection.frame.1, text = 'regression with external variable(s)', var = calc.analysis, value = 'reg', command = function (...) {tkconfigure (radio.pearson, state = 'disabled'); tkconfigure (radio.kendall, state = 'disabled'); tkconfigure (radio.spearman, state = 'disabled'); tkconfigure (radio.E_IV, state = 'active'); tkconfigure (radio.IV_E, state = 'active')}),	
        tkradiobutton (selection.frame.1, text = 'multiple regression with external variables\n(e.g. with ordination axes)', var = calc.analysis, value = 'mult.reg', justify = 'left', command = function (...) {tkconfigure (radio.pearson, state = 'disabled'); tkconfigure (radio.kendall, state = 'disabled'); tkconfigure (radio.spearman, state = 'disabled'); tkconfigure (radio.E_IV, state = 'disabled'); tkconfigure (radio.IV_E, state = 'disabled')}),
        anchor = 'w', side = 'top')


radio.pearson <- tkradiobutton (selection.frame.2, text = 'Pearson', var = cor.method, value = 'pearson', state = 'disabled')
radio.kendall <- tkradiobutton (selection.frame.2, text = 'Kendall', var = cor.method, value = 'kendall', state = 'disabled')
radio.spearman <- tkradiobutton (selection.frame.2, text = 'Spearman', var = cor.method, value = 'spearman', state = 'disabled')
tkpack (tklabel (selection.frame.2, text = 'As correlation coefficient use:'), radio.pearson, radio.kendall, radio.spearman, anchor = 'w', side = 'top')

radio.E_IV <- tkradiobutton (selection.frame.3, text = 'External ~ mean (IV)', var = reg.direction, value = 'E-IV', state = 'disabled')
radio.IV_E <- tkradiobutton (selection.frame.3, text = 'mean (IV) ~ External', var = reg.direction, value = 'IV-E', state = 'disabled')
tkpack (tklabel (selection.frame.3, text = 'Dependence of variables in regression \n(dependent ~ independent):'), radio.E_IV, radio.IV_E, anchor = 'w', side = 'top')

tkpack (tklabel (selection.frame.4, text = 'As species attributes use:'),
        tkradiobutton (selection.frame.4, text = 'species Ellenberg values', var = species.data, value = 'EIVs'),
        tkradiobutton (selection.frame.4, text = 'species data', var = species.data, value = 'sp.data'),
        anchor = 'w', side = 'top')

par.test.button <- tkcheckbutton (selection.frame.5, text = 'parametric test', var = P.par.test)
perm.test.button <- tkcheckbutton (selection.frame.5, text = 'original permutation test', var = P.perm.test)
modif.test.button <- tkcheckbutton (selection.frame.5, text = 'modified permutation test', var = P.modif.test)
tkpack (tklabel (selection.frame.5, text = 'Calculate following tests:'), par.test.button, perm.test.button, modif.test.button, side = 'top', anchor = 'w')


tkpack (tklabel (selection.frame.6, text = 'Number of permutations:'), tkentry (selection.frame.6, width = 8, textvariable = randomizations), side = 'top', anchor = 'w')

tkpack (selection.frame.1, selection.frame.2, selection.frame.3, anchor = 'w', expand = T, fill = 'both', side = 'left', pady = 5, padx = 5)
tkpack (selection.frame.4, selection.frame.5, selection.frame.6, anchor = 'w', expand = T, fill = 'both', side = 'left', pady = 5, padx = 5)
tkpack (selection.frame.up, selection.frame.bottom, side = 'top', expand = T, fill = 'both')
tkpack (selection.frame, expand = T, fill = 'both', side = 'top', anchor = 'w' )

button.calculate <- tkbutton (button.frame.2, text = 'Calculate', background = 'lightgrey', command = function ()
  if (tclvalue (calc.analysis) == 'ANOVA') {simple.data.check (calc.analysis = tclvalue (calc.analysis), sp.data = tclvalue (species.data)); calc.anova (as.numeric (tclvalue (randomizations)), species.data = tclvalue (species.data))} else 
    if (tclvalue (calc.analysis) == 'cor') {simple.data.check (calc.analysis = tclvalue (calc.analysis), sp.data = tclvalue (species.data)); calc.cor (as.numeric (tclvalue (randomizations)), species.data = tclvalue (species.data))} else
      if (tclvalue (calc.analysis) == 'reg') {simple.data.check (calc.analysis = tclvalue (calc.analysis), sp.data = tclvalue (species.data)); calc.reg (as.numeric (tclvalue (randomizations)), species.data = tclvalue (species.data))} else
        if (tclvalue (calc.analysis) == 'mult.reg') {simple.data.check (calc.analysis = tclvalue (calc.analysis), sp.data = tclvalue (species.data)); calc.mult.reg (as.numeric (tclvalue (randomizations)), species.data = tclvalue (species.data))})
button.exit <- tkbutton (button.frame.2, text = 'Exit', command = function () {tkdestroy (base); tclvalue (cancel) <- 1})

tkpack (button.calculate, button.exit, side = 'right', anchor = 'e', padx = 5, pady = 5)
tkpack (button.frame.2, anchor = 'w', side = 'right', pady = 10, padx = 10)

button.onlinehelp <- tkbutton (button.frame.1, text = 'Online help',  command = function () browseURL ('http://www.sci.muni.cz/botany/zeleny/wiki/david-wiki/doku.php?id=eiv:software'))
button.table <- tkbutton (button.frame.1, text = 'View result table', command = function () try(shell.exec (paste (getwd(), '/iv_result_table.txt', sep = ''))))

tkpack (button.onlinehelp, button.table, padx = 5, pady = 5, side = 'left')
tkpack (button.frame.1, side = 'left')

tkpack (button.frame, side = 'right', anchor = 'w', expand = T, fill = 'both')

tkraise (base)
#bringToTop (dev.cur(), stay = F)
tkbind (base, '<Destroy>', function()tclvalue(cancel)<-2)
tkwait.variable (cancel)
}


if (exists ('JUICE.table') & exists ('JUICE.short.head') & exists ('JUICE.species.data'))
{
  veg <- JUICE.table
  spec.iv <- clean.ell (JUICE.species.data)[,c("Ellenberg_Light", "Ellenberg_Temperature", "Ellenberg_Continentality", "Ellenberg_Moisture", "Ellenberg_Soil_Reaction", "Ellenberg_Nutrients")]
  rownames (spec.iv) <- JUICE.species.data[,1]
  spec.data <- JUICE.species.data[,'Species_Data', drop = F]
  rownames (spec.data) <- JUICE.species.data[,1]
  groups <- JUICE.short.head[, 'Group_Number', drop = F]
  rownames (groups) <- JUICE.short.head$Releve_Number
  external <- JUICE.short.head [, 'Short_Head', drop = F]
  rownames (external) <- JUICE.short.head$Releve_Number
  tkconfigure (open.veg.lab, text = 'Data imported from JUICE')
  tkconfigure (open.speciv.lab, text = 'Data imported from JUICE')
  tkconfigure (open.specdata.lab, text = 'Data imported from JUICE')
  tkconfigure (open.groups.lab, text = 'Data imported from JUICE')
  tkconfigure (open.external.lab, text = 'Data imported from JUICE')
}

