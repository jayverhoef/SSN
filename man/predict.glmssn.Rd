\name{predict.glmssn}
\alias{predict.glmssn}
\title{
Calculate Predictions for Prediction Sites
}
\description{
The \command{predict.glmssn} function calculates prediction values for prediction
sites based on the results stored within an object of class glmssn
}
\usage{
\method{predict}{glmssn}(object, predpointsID, ...)
}
\arguments{
  \item{object}{
    the glmssn object
  }
  \item{predpointsID}{
    the prediction points ID
  }
  \item{...}{
    other arguments passed to predict
  }
}
\details{
  \command{predict.glmssn} is a generic function that has been modified for
  \code{glmssn} objects. Two new columns are added to the prediction points
  data.frame (point.data) within the existing \code{glmssn} object. The first
  column contains prediction values and has the same name as the response variable.
  The second column <response name>.predSE contains the standard errors for the
  predictions.

}
\value{
  The overall structure is the same as an object of class \code{glmssn}, except
  the prediction points data.frame in the SpatialStreamNetwork object (list-item 2) is appended by the
  following columns:
  \preformatted{
    <response name> # The prediction value for each prediction site
    <response name>.predSE # The standard error of the prediction value
  }
  Details of this object structure can be found using the \code{names}
  command.
}
\author{
Jay Ver Hoef \email{support@SpatialStreamNetworks.com}
}

\seealso{
  \code{\link{glmssn}}
}
\examples{
library(SSN)
data(modelFits)
#Update the fitSp path, will vary for each users installation
fitSp$ssn.object <- updatePath(fitSp$ssn.object, 
  system.file("lsndata/MiddleFork04.ssn", package = "SSN"))
names(fitSp)
names(fitSp$ssn.object)

ssnpred <- predict(fitSp, predpointsID = "pred1km")
names(ssnpred)
names(ssnpred$ssn.object) ## Note additional predicted columns in pred1km
plot(ssnpred)


}
