\name{icp.matrix}
\alias{icp.matrix}
\title{ICP matching for two matrices}
\usage{
  icp.matrix(x, y, maxdist = 1, weighting = TRUE,
    iterations = 3)
}
\arguments{
  \item{x}{matrix}

  \item{y}{matrix containing target coordinates}

  \item{maxdist}{maximum distance to consider}

  \item{weighting}{logical if TRUE, each point is weighted
  by the inverse distance to the closest point}

  \item{iterations}{integer: iterations to run}
}
\value{
  returns scaled, translated and rotated version of
  \code{x}
}
\description{
  ICP matching for two matrices
}
\examples{
require(Morpho)
data(nose)
matchpoint <- icp.matrix(longnose.lm,shortnose.lm, maxdist = 5)
}

