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
	mf04p <- importSSN(system.file("lsndata/MiddleFork04.ssn", package = "SSN"),
		  predpts = "pred1km")
	mf04p <- importPredpts(target = mf04p, predpts = "CapeHorn", obj.type = "ssn")
	mf04p <- importPredpts(target = mf04p, predpts = "Knapp", obj.type = "ssn")
	names(mf04p)
}
