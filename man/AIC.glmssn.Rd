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
# NOT RUN 
#mf04p <- importSSN(system.file("lsndata/MiddleFork04.ssn", 
#  package = "SSN"), predpts = "pred1km", o.write = TRUE)
#  use SpatialStreamNetwork object mf04p that was already created
data(mf04p)
# for examples, copy MiddleFork04.ssn directory to R's temporary directory
copyLSN2temp()
#make sure mf04p has the correct path, will vary for each users installation
mf04p <- updatePath(mf04p, paste0(tempdir(),'/MiddleFork04.ssn'))
   
# get some model fits stored as data objects
data(modelFits)
#NOT RUN use this one
#fitSp <- glmssn(Summer_mn ~ ELEV_DEM + netID,
#    ssn.object = mf04p, EstMeth = "REML", family = "Gaussian",
#    CorModels = c("Exponential.tailup","Exponential.taildown",
#    "Exponential.Euclid"), addfunccol = "afvArea")
#make sure fitSP has the correct path, will vary for each users installation
fitSp$ssn@path <- paste0(tempdir(),'/MiddleFork04.ssn')

#note the model was fitted using REML, so fixed effects have 
# been integrated out
summary(fitSp)
AIC(fitSp)

}
