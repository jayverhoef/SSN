\name{GR2}
\alias{GR2}
\title{
Generalised R2
}
\description{
  \command{GR2} is a generic function that calculates and displays the
  generalised R2 value for fitted \code{glmssn} objects.
}
\usage{
  GR2(object)
}

\arguments{
  \item{object}{
    an object of class \link{glmssn}
  }
}
\details{
  The generalised R2 value, or \sQuote{coefficient of
  determination}, lies somewhere between 0 and 1, and is a general measure of
  the predictive power of a model. In this instance, it relates to the
  proportion of the variability in the data that is explained by the 
  fixed effects.
}
\value{
  a numeric value equal to the GR2 for the specified \code{glmssn} object
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

GR2(fitSp)

}
