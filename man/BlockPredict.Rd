\name{BlockPredict}
\alias{BlockPredict}

\title{
Block Predicton for Streams Data}
\description{
Block prediction for objects of class \link{glmssn-class}
}
\usage{
BlockPredict(object, predpointsID)
}

\arguments{
  \item{object}{
    an object of class \link{glmssn}
  }
  \item{predpointsID}{
    a valid prediction points ID
  }
}
\details{
  This function operates on \code{glmssn} objects in much the same
  way as the \command{predict} function. \command{BlockPredict} uses the
  locations in the predpointsID data set to compute the average prediction
  value in the area defined by the prediction locations.  These prediction
  locations are used to approximate the integral over that area, so they
  should be evenly spaced and dense in the area where block prediction is
  desired.  The user needs to create these prediction locations and include
  them in the SSN object prior to fitting the model with glmssn.
}
\value{
  A data.frame with one row and two columns.  The first column, BlockPredEst,
  is the average prediction value, and the second column, BlockPredSE, is the
  standard error of the block prediction.
}
\references{
Ver Hoef, J. M.. Peterson, E. E. and Theobald, D. (2006) Spatial statistical
  models that use flow and stream distance. \emph{Environmental and
  Ecological Statistics} \bold{13}, 449-464. DOI: 10.1007/s10651-006-0022-8.
}
\author{
Jay Ver Hoef \email{support@SpatialStreamNetworks.com}
}

\examples{

\dontrun{
# NOT RUN 
library(SSN)
# use SpatialStreamNetwork object mf04 that was already created
data(mf04p)
# for examples, copy MiddleFork04.ssn directory to R's temporary directory
copyLSN2temp()
#make sure mf04p has the correct path; R's tmp directory
mf04p <- updatePath(mf04p, paste0(tempdir(),'/MiddleFork04.ssn'))

# Not needed: already added,
# add densely gridded prediction points for two stream segments
# mf04p <- importPredpts(mf04p, "Knapp", "ssn")
# mf04p <- importPredpts(mf04p, "CapeHorn", "ssn")
names(mf04p)

# see densely gridded prediction points on stream
plot(mf04p, PredPointsID = "Knapp")

#  you could fit the model
#fitSpBk <- glmssn(Summer_mn ~ ELEV_DEM + netID,
#    ssn.object = mf04p, EstMeth = "REML", family = "Gaussian",
#    CorModels = c("Exponential.tailup","Exponential.taildown",
#    "Exponential.Euclid"), addfunccol = "afvArea")

# or load the pre-fit model
data(modelFits)
fitSpBk$ssn.object <- updatePath(fitSpBk$ssn.object, 
	paste0(tempdir(),'/MiddleFork04.ssn'))

# one-at-a-time predictions for CapeHorn stream
## NOTE: need the amongpreds distance matrices for block prediction
createDistMat(mf04p, predpts = "CapeHorn", o.write = TRUE, amongpreds = TRUE)
fitSpPredC <- predict(fitSpBk, "CapeHorn")
# plot densely gridded prediction points on stream
plot(fitSpPredC, "Summer_mn")
# block prediction for CapeHorn stream
BlockPredict(fitSpBk, "CapeHorn")

## Another example
# one-at-a-time predictions for Knapp stream
createDistMat(mf04p, predpts = "Knapp", o.write = TRUE, amongpreds = TRUE)
fitSpPredK <- predict(fitSpBk, "Knapp")
# plot densely gridded prediction points on stream
plot(fitSpPredK, "Summer_mn")
# block prediction for Knapp stream
BlockPredict(fitSpBk, "Knapp")
}
}

