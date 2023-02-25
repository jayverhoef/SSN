\name{AIC}
\alias{AIC.glmssn}
\title{
AIC for glmssn objects
}
\description{
  \command{AIC.glmssn} is a method that calculates AIC for fitted \code{glmssn} objects.
}
\usage{
  \method{AIC}{glmssn}(object, ..., k = 2)
}

\arguments{
  \item{object}{
    an object of class \link{glmssn}
  }
  \item{...}{
    optionally more fitted model objects
  }
  \item{k}{
    numeric, the penalty per parameter to be used; 
    the default k = 2 is the classical AIC. 
  }
}
\details{
  \link{AIC} is a generic function and this implements a method for \code{glmssn} objects
}
\value{
  a numeric AIC value for the specified \code{glmssn} object
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

#note the model was fitted using REML, so fixed effects have 
# been integrated out
summary(fitSp)
AIC(fitSp)

}
