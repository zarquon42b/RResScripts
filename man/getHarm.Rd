% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EFAhelper.r
\name{getHarm}
\alias{getHarm}
\title{get harmonics}
\usage{
getHarm(x, nh = NULL, type = c("list", "indices", "matrix"))
}
\arguments{
\item{x}{\code{\link{Coe}} object}

\item{nh}{integer (vector): number of harmonics to extract}

\item{type}{type="list" returns a list of coefficients, type="matrix" a matrix, and type="index" the column indices.}
}
\value{
list or matrix with harmonics (an, bn, cn, dn), or list with column indices
}
\description{
get harmonvics
}

