\name{mf04p}
\alias{mf04p}
\docType{data}
\title{
	Imported SpatialStreamNetwork object from MiddleFork04.ssn data folder
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
	mf04p <- importSSN(paste0(tempdir(),'/MiddleFork04.ssn'), predpts = "pred1km",
		o.write = TRUE)
	mf04p <- importPredpts(target = mf04p, predpts = "CapeHorn", obj.type = "ssn")
	mf04p <- importPredpts(target = mf04p, predpts = "Knapp", obj.type = "ssn")
	names(mf04p)
}
