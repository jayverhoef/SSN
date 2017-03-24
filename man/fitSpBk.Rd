\name{fitSpBk}
\alias{fitSpBk}
\docType{data}
\title{
	Fitted glmssn object for example data set MiddleFork.ssn
}
\description{
  The MiddleFork04.ssn data folder contains the spatial, attribute, and topological
  information needed to construct a spatial stream network object using the SSN
  package.  This is a fitted model using the \code{\link{glmssn}} function. It
	is used for the block prediction example.
}
\details{
	See the help for \code{\link{glmssn}} for how the model was created, and \code{\link{BlockPredict}} for usage in block prediction.
}

\examples{
	library(SSN)
	data(modelFits)
	ls()
}
