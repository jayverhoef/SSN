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
#for examples, copy MiddleFork04.ssn directory to R's temporary directory
copyLSN2temp()
mf04 <- importSSN(paste0(tempdir(),'/MiddleFork04.ssn'), o.write = TRUE)
names(mf04)
}
