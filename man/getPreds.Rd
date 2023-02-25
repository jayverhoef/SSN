\name{getPreds}
\alias{getPreds}
\title{
Extract Predictions with associated standard errors.
}
\description{
This function extracts predictions and standard errors from objects of class
'glmssn.predict' or 'influenceSSN'. Predictions are extracted for unobserved
locations, while cross-validation predictions are extracted for observed locations.

}
\usage{
getPreds(x, pred.type = "cv")
}
\arguments{
  \item{x}{
    an object of class \link{predict.glmssn} or \link{influenceSSN-class}
}
  \item{pred.type}{
    prediction type, either "pred" or "cv". The "pred" option indicates that a
    'glmssn.predict' object is being accessed and a text file containing
    predictions and standard errors for the predictions is exported. When the
    "cv" option is used, objects of class influenceSSN are accessed and
    cross-validation predictions and standard errors are exported.
}
}

\value{
  \code{getPreds} returns a matrix containing the point identifier (pid), the
  predictions, and the standard errors for the predictions.
}

\author{
Erin E. Peterson \email{support@SpatialStreamNetworks.com}
}

\seealso{
  \code{\link{predict}}, \code{\link{influenceSSN-class}}
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
#Update the path in fitSp, will vary for each users installation
fitSp$ssn.object <- updatePath(fitSp$ssn.object, 
  paste0(tempdir(),'/MiddleFork04.ssn'))

# Extract predictions and standard errors for the prediction sites
# make sure the distance matrix is there
createDistMat(mf04p, predpts = "pred1km", o.write = TRUE)
#create predictions
fitSpPred <- predict(fitSp, predpointsID = "pred1km")
class(fitSpPred)
fitSpgetPreds <- getPreds(fitSpPred, pred.type = "pred")
head(fitSpgetPreds)

# Extract cross-validation predictions for the observed sites in two ways:
fitSpRes <- residuals(fitSp)
class(fitSpRes)

# Extract from the influenceSSN class object
fitSpResGetCV <- getPreds(fitSpRes, pred.type = "cv")
head(fitSpResGetCV)

# Extract from the glmssn.predict class object
fitSpResGetCV2 <- getPreds(fitSpPred, pred.type = "cv")
# These values are identical
identical(fitSpResGetCV,fitSpResGetCV) ## TRUE

}
