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
# NOT RUN 
#mf04p <- importSSN(system.file("lsndata/MiddleFork04.ssn", 
#  package = "SSN"), predpts = "pred1km", o.write = TRUE)
#  use SpatialStreamNetwork object mf04p that was already created
data(mf04p)
#make sure mf04p has the correct path, will vary for each users installation
mf04p@path <- system.file("lsndata/MiddleFork04.ssn", package = "SSN")
    
# get some model fits stored as data objects
data(modelFits)
#NOT RUN use this one
#fitSp <- glmssn(Summer_mn ~ ELEV_DEM + netID,
#    ssn.object = mf04p, EstMeth = "REML", family = "Gaussian",
#    CorModels = c("Exponential.tailup","Exponential.taildown",
#    "Exponential.Euclid"), addfunccol = "afvArea")
#make sure fitSP has the correct path, will vary for each users installation
fitSp$ssn@path <- system.file("lsndata/MiddleFork04.ssn", package = "SSN")

GR2(fitSp)

}
