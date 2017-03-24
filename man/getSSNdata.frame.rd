\name{getSSNdata.frame}
\alias{getSSNdata.frame}
\title{Extract data from SSN objects as a data.frame}
\description{
  The \command{getSSNdata.frame} function extracts the points data data.frame,
  either observation data or prediction data, from the specified SSN object.
}
\usage{
getSSNdata.frame(x, Name = "Obs")
}
\arguments{
  \item{x}{
    an object of class \link{SpatialStreamNetwork-class}, \link{influenceSSN-class},
    \link{glmssn-class}, or "glmssn.predict".
  }
  \item{Name}{
    the internal name of the data set in the object \code{x}. For observed values,
    this will always be "Obs", the default.
  }
}
\details{
The internal \code{Name} for observed data in objects of class \code{SpatialStreamNetwork}
is "Obs" and it is the default. If another \code{Name} is specified, it must
represent a prediction data set in the \code{SpatialStreamNetwork-class},
\code{influenceSSN-class}, \code{glmssn-class}, or "glmssn.predict" object. For
\code{SpatialStreamNetwork} objects, these names are obtained using the call
ssn@predpoints@ID. For all other object classes, the names are
obtained using the call object$ssn.object@predpoints@ID. See examples for additional
details.
}
\value{
  A \code{\link{data.frame}}.
}

\author{
Jay Ver Hoef \email{support@SpatialStreamNetworks.com}
}
\seealso{
	\code{\link{putSSNdata.frame}}
}

\examples{
library(SSN)
# NOT RUN
#mf04 <- importSSN(system.file("lsndata/MiddleFork04.ssn",
#  package = "SSN"), o.write = TRUE)
#  use SpatialStreamNetwork object mf04p that was already created
data(mf04)
#Update path in mf04, will vary for each users installation
mf04 <- updatePath(mf04, system.file("lsndata/MiddleFork04.ssn", package = "SSN"))

obsDF <- getSSNdata.frame(mf04)
head(obsDF)

# get some model fits stored as data objects
data(modelFits)
#NOT RUN use this one
#fitSp <- glmssn(Summer_mn ~ ELEV_DEM + netID,
#    ssn.object = mf04p, EstMeth = "REML", family = "Gaussian",
#    CorModels = c("Exponential.tailup","Exponential.taildown",
#    "Exponential.Euclid"), addfunccol = "afvArea")
#Update path for fitSP, will vary for each users installation
fitSp$ssn.object <- updatePath(fitSp$ssn,system.file("lsndata/MiddleFork04.ssn", package = "SSN"))

# Get the data.frame from an influenceSSN object and plot the residuals
fitSpRes <- residuals(fitSp)
fitSpResDF <- getSSNdata.frame(fitSpRes)
# NOT RUN
#plot(fitSpResDF[,"_resid.crossv_"],fitSpResDF[,"_resid_"], pch = 19,
#  ylab = "Cross-validation Residuals", xlab = "Raw Residuals")

# Get the data.frame for the prediction locations
fitSpPred <- predict(fitSp, predpointsID = "pred1km")
predNames<- fitSpPred$ssn.object@predpoints@ID
fitSpPredDF <- getSSNdata.frame(fitSpPred, predNames[1])
head(fitSpPredDF)

}

