\name{mf04}
\alias{mf04}
\docType{data}
\title{
imported SpatialStreamNetwork object from MiddleFork04.ssn data folder
}
\description{
  The MiddleFork04.ssn data folder contains the spatial, attribute, and topological
  information needed to construct a spatial stream network object using the SSN
  package.  This is the representation by using the importSSN function.
}
\details{
	See the \code{\link{MiddleFork04.ssn}} for details of data set, \code{\link{importSSN}} on how to get it
	into SSN.
}
\source{
  See the \code{\link{MiddleFork04.ssn}}
}

\examples{
library(SSN)
mf04 <- importSSN(system.file("lsndata/MiddleFork04.ssn", package = "SSN"))
names(mf04)
}
