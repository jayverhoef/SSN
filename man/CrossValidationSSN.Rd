\name{CrossValidationSSN}
\alias{CrossValidationSSN}
\title{
Compute Crossvalidation Values for glmssn Objects
}
\description{
  \code{CrossValidationSSN} operates on \link{glmssn} objects. The response values
  are removed one at a time and the estimated model is used to predict
  each of the removed values along with the standard errors of prediction.

}
\usage{
  CrossValidationSSN(object)
}
\arguments{
  \item{object}{
    an object of class \link{glmssn-class}
  }

}
\details{
  This function removes the response values one at a time.  Then it uses the
  estimated model to predict each of the removed values along with the standard
  errors of prediction.
}
\value{
  Output is a data.frame with three columns, the point identifier "pid",
  predictions "cv.pred", and
  their standard errors "cv.se". The data are in the same order as the data
  in the glmssn object.
}

\author{
  Jay Ver Hoef \email{support@SpatialStreamNetworks.com}
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

fitSpCrVal <- CrossValidationSSN(fitSp)
str(fitSpCrVal)
# NOT RUN
# data are sorted by netID, then pid within netID.  This is different that
# the original data order, so get the sorted values of the response variable
# plot(fitSp$sampinfo$z, fitSpCrVal[,"cv.pred"], pch = 19)

}
