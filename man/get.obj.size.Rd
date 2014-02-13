\name{get.obj.size}
\alias{get.obj.size}
\title{analyse memory usage of objects in current workspace}
\usage{
get.obj.size(units = 2)
}
\arguments{
  \item{units}{decimal power to show used space e.g.
  2=1024^2 bytes = Megabytes.}
}
\value{
data frame of objects and space used
}
\description{
analyse memory usage of objects in current workspace
}
\examples{
get.obj.size()
}

