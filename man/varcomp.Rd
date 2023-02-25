\name{varcomp}
\alias{varcomp}
\title{
Variance Components for glmssn Objects
}
\description{
  The \command{varcomp} function displays the variance proportions for the
  autocovariance functions in the \code{glmssn} object.
}
\usage{
varcomp(object)
}
\arguments{
  \item{object}{
  a \link{glmssn-class} object
}
}
\details{
 The \command{varcomp} function displays the variance component for
 each autocovariance function, as well as the \code{Covariates} component and
 the \code{nugget}.
}
\value{
A data frame with two columns. The first column is the name of the variance component
and the second column is the proportion for each variance component.
}

\author{
Jay Ver Hoef \email{support@SpatialStreamNetworks.com}
}

\seealso{
  \code{\link{glmssn}}, \code{\link{covparms}}
}
\examples{

	library(SSN)
	#for examples, copy MiddleFork04.ssn directory to R's temporary directory
	copyLSN2temp()
	# NOT RUN
	# Create a SpatialStreamNetork object that also contains prediction sites
	#mf04p <- importSSN(paste0(tempdir(),'/MiddleFork04.ssn'), 
	#  predpts = "pred1km", o.write = TRUE)
	#use mf04p SpatialStreamNetwork object, already created
	data(mf04p)
	#for examples only, make sure mf04p has the correct path
	#if you use importSSN(), path will be correct
	mf04p <- updatePath(mf04p, paste0(tempdir(),'/MiddleFork04.ssn'))
		  
	# get some model fits stored as data objects
	data(modelFits)
	#NOT RUN use this one
	#fitSp <- glmssn(Summer_mn ~ ELEV_DEM + netID,
	#    ssn.object = mf04p, EstMeth = "REML", family = "Gaussian",
	#    CorModels = c("Exponential.tailup","Exponential.taildown",
	#    "Exponential.Euclid"), addfunccol = "afvArea")
	#for examples only, make sure fitSp has the correct path
	#if you use importSSN(), path will be correct
	fitSp$ssn.object <- updatePath(fitSp$ssn.object, 
		paste0(tempdir(),'/MiddleFork04.ssn'))

	varcomp(fitSp)

}
