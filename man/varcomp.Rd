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
	# NOT RUN 
	#mf04p <- importSSN(system.file("lsndata/MiddleFork04.ssn", 
	#  package = "SSN"), predpts = "pred1km", o.write = TRUE)
	#  use SpatialStreamNetwork object mf04p that was already created
	data(mf04p)
	#make sure mf04p has the correct path, will vary for each users installation
	mf04p@path <- system.file("lsndata/MiddleFork04.ssn", package = "SSN")
		  
	# get some model fits stored as data objects
	data(modelFits)
	#NOT RUN use this one
	#fitSp <- glmssn(Summer_mn ~ ELEV_DEM + netID,
	#    ssn.object = mf04p, EstMeth = "REML", family = "Gaussian",
	#    CorModels = c("Exponential.tailup","Exponential.taildown",
	#    "Exponential.Euclid"), addfunccol = "afvArea")
	#make sure fitSP has the correct path, will vary for each users installation
	fitSp$ssn@path <- system.file("lsndata/MiddleFork04.ssn", package = "SSN")

	varcomp(fitSp)

}
