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

obsDF <- getSSNdata.frame(mf04)
head(obsDF)

# get some model fits stored as data objects
data(modelFits)
#NOT RUN use this one
#fitSp <- glmssn(Summer_mn ~ ELEV_DEM + netID,
#    ssn.object = mf04p, EstMeth = "REML", family = "Gaussian",
#    CorModels = c("Exponential.tailup","Exponential.taildown",
#    "Exponential.Euclid"), addfunccol = "afvArea")
#for examples only, make sure fitSp has the correct path
#if you use importSSN(), path will be correct
fitSp$ssn.object <- updatePath(fitSp$ssn.object, 
	paste0(tempdir(),'/MiddleFork04.ssn'))

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

