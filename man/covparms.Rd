
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
#for examples only, make sure fitSp has the correct path
#if you use importSSN(), path will be correct
fitSp$ssn.object <- updatePath(fitSp$ssn.object, 
	paste0(tempdir(),'/MiddleFork04.ssn'))

## Look at variance components in more detail
covparms(fitSp)
}
