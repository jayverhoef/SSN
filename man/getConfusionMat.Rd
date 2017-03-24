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

# get some model fits stored as data objects
data(modelFits)

## NOT RUN
## Fit a model to binary data
## binSp <- glmssn(MaxOver20 ~ ELEV_DEM + SLOPE, mf04p,
##   CorModels = c("Mariah.tailup", "Spherical.taildown"),
##   family = "binomial", addfunccol = "afvArea")

## make sure binSp has the correct path, will vary for
## each users installation
binSp$ssn.object <- updatePath(binSp$ssn.object, 
	system.file("lsndata/MiddleFork04.ssn", package = "SSN"))
summary(binSp)

## Generate the leave-one-out cross-validation predictions
## for the observed sites.
binResids <- residuals(binSp, cross.validation = TRUE)

## Generate the confusion matrix for the binary spatial
## model, based on the observations and leave-one-out
## cross-validation predictions
getConfusionMat(binResids, threshold = 0.5)
}
