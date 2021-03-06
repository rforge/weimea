% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fourth.corner.r
\name{fourth.corner}
\alias{fourth.corner}
\alias{print.fc}
\alias{summary.fc}
\alias{fourth.corner.ade}
\title{calculates the fourth-corner statistic (according to Legendre et al. 1997, compiled according to Peres-Neto et al. 2012)}
\usage{
fourth.corner(R, L, Q, chessel = FALSE)

\method{print}{fc}(x, ...)

\method{summary}{fc}(object, ...)

fourth.corner.ade(sitspe, speatt, env, fc.test, perm, chessel = FALSE)
}
\arguments{
\item{R, env}{Matrix of sample attributes.}

\item{L, sitspe}{Matrix of species composition.}

\item{Q, speatt}{Matrix of species attributes}

\item{chessel}{Logical; should the Chessel's variant of the fourth corner statistic be returned? Default is \code{FALSE}}

\item{x, object}{Object of 'cwm' class.}

\item{...}{Other arguments passing into \code{print} or \code{summary} functions (not implemented yet).}

\item{fc.test}{Test to be chosen for fourthcorner analysis.}

\item{perm}{Number of permutations.}
}
\description{
calculates the fourth-corner statistic (according to Legendre et al. 1997, compiled according to Peres-Neto et al. 2012)
}
\details{
Chessel's version of the fourth corner statistic has been introduced by Peres-Neto et al. (2016), to rescale the fourth corner r statistic into the range of -1 and 1. This is achieved by dividing the observed fourth corner r by square root of the first eigenvalue for correspondance analysis of L (sitspe) matrix.
}
