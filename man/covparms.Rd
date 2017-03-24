
\name{covparms}
\alias{covparms}

\title{
Get Covariance Parameters
}
\description{
Displays the covariance parameter estimates for the autocovariance function(s)
in the \code{glmssn} object.
}
\usage{
covparms(object)
}
\arguments{
  \item{object}{
  An object of class \link{glmssn-class} created using the \command{glmssn} 
  function.
}
}
\details{
	A \link{glmssn-class}object allows up to three autocovariance functions, as well
	as random effects. The \command{covparms}
	function displays the sill and range for each model, as well as the nugget.
}
\value{
	Returns a \code{data.frame} containing the names of the autocovariance functions 
	and random effects, the parameter names, and their corresponding estimates.
}

\author{
	Jay Ver Hoef \email{support@SpatialStreamNetworks.com}
}

\seealso{
  \code{\link{glmssn}}

}
\examples{
	library(SSN)
	# NOT RUN 
	# mf04 <- importSSN(system.file("lsndata/MiddleFork04.ssn", 
	#	        package = "SSN"), o.write = TRUE)
	# use SpatialStreamNetwork object mf04 that was already created
	data(mf04)

# for examples, copy MiddleFork04.ssn directory to R's temporary directory
copyLSN2temp()
#make sure mf04p has the correct path, will vary for each users installation
mf04 <- updatePath(mf04, paste0(tempdir(),'/MiddleFork04.ssn'))



	## NOT RUN Distance Matrix has already been created
	## createDistMat(mf04)

	# The models take a little time to fit, so they are NOT RUN 
	# Uncomment the code to run them
	# Alternatively, you can load the fitted models first to look at results
	data(modelFits)

	## 3 component spatial model
	#fitSp <- glmssn(Summer_mn ~ ELEV_DEM + netID,
	#    ssn.object = mf04, EstMeth = "REML", family = "Gaussian",
	#    CorModels = c("Exponential.tailup","Exponential.taildown",
	#    "Exponential.Euclid"), addfunccol = "afvArea")

	## Look at variance components in more detail
	covparms(fitSp)
}
