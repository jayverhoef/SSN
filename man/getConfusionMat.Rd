\name{getConfusionMat}
\alias{getConfusionMat}
\title{
Extract confusion matrix for binary spatial model predictions.
}
\description{
This function extracts the binary confusion matrix from 'influenceSSN' objects.
The matrix values are based on a leave-one-out cross-validation of the
observed dataset.
}
\usage{
getConfusionMat(x, threshold = 0.5)
}
\arguments{
  \item{x}{
    an object of class \link{influenceSSN-class}
}
  \item{threshold}{
    a numeric value used to classify binary spatial model predictions
    into 1's and 0's. The default is 0.5.
}
}

\value{ \code{getConfusionMat} returns a 2x2 matrix containing
  information about the classification accuracy of the binary spatial
  model, based on the observations and the leave-one-out
  cross-validation predictions.
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
#mf04 <- importSSN(paste0(tempdir(),'/MiddleFork04.ssn', o.write = TRUE))
#use mf04 SpatialStreamNetwork object, already created
data(mf04)
#for examples only, make sure mf04p has the correct path
#if you use importSSN(), path will be correct
mf04 <- updatePath(mf04, paste0(tempdir(),'/MiddleFork04.ssn'))

# get some model fits stored as data objects
data(modelFits)

## NOT RUN
## Fit a model to binary data
## binSp <- glmssn(MaxOver20 ~ ELEV_DEM + SLOPE, mf04p,
##   CorModels = c("Mariah.tailup", "Spherical.taildown"),
##   family = "binomial", addfunccol = "afvArea")

#for examples only, make sure binSp has the correct path
#if you use importSSN(), path will be correct
binSp$ssn.object <- updatePath(binSp$ssn.object, 
	paste0(tempdir(),'/MiddleFork04.ssn'))
summary(binSp)

## Generate the leave-one-out cross-validation prediction residuals
## for the observed sites.
binResids <- residuals(binSp, cross.validation = TRUE)

## Generate the confusion matrix for the binary spatial
## model, based on the observations and leave-one-out
## cross-validation predictions
getConfusionMat(binResids, threshold = 0.5)
}
