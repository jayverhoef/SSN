\name{BLUP}
\alias{BLUP}

\title{
Compute the joint mean, variance and covariance of any random effects
in a glmssn model conditional on the data
}

\description{
Compute the joint mean, variance and covariance of any random effects
in a glmssn model conditional on the data. This assumes each random
effect has a Gaussian distribution with mean zero and covariance
matrix sigma^2 * Identity. We just plug in the REML estimate of
sigma^2 from the fitted glmssn model object.
}

\usage{
BLUP(model, RE = NULL)
}

\arguments{
  \item{model}{
  An object of class \link{glmssn-class}
  }
  \item{RE}{
  Names of random effects (RE), defaults to all REs in the \code{glmssn} object,
  if any
  }
}

\details{
Similar to BLUP in the regress package.
}
\value{
  \item{Mean}{
  A vector of means for each Random Effect
  }
  \item{Variance}{
  A vector of variances for each Random Effect
  }
  \item{Covariance}{
  A variance-covariance matrix for the Random Effects
  }
}

\author{
David Clifford \email{support@SpatialStreamNetworks.com}
}

\examples{

library(SSN)
# NOT RUN 
# mf04 <- importSSN(system.file("lsndata/MiddleFork04.ssn", 
#	        package = "SSN"), o.write = TRUE)
# use SpatialStreamNetwork object mf04 that was already created
data(mf04)
# for examples, copy MiddleFork04.ssn directory to R's temporary directory
copyLSN2temp()
#make sure mf04p has the correct path, will vary for each users installation
mf04 <- updatePath(mf04, paste0(tempdir(),'/MiddleFork04.ssn'))

## NOT RUN Distance Matrix has already been created
## createDistMat(mf04)

# The models take a little time to fit, so they are NOT RUN 
# Uncomment the code to run them
# Alternatively, you can load the fitted models first to look at results
data(modelFits)

## Random effect model using STREAMNAME as our random effect
#fitRE <- glmssn(Summer_mn ~ ELEV_DEM + netID,
#    ssn.object = mf04, EstMeth = "REML", family = "Gaussian",
#    CorModels = c("STREAMNAME"))
summary(fitRE)
## random effects details
fitREBLUP <- BLUP(fitRE)
str(fitREBLUP)
fitREBLUP$Mean

## spatial stream model with a random effect
#fitSpRE1 <- glmssn(Summer_mn ~ ELEV_DEM + netID,
#    ssn.object = mf04, EstMeth = "REML", family = "Gaussian",
#    CorModels = c("STREAMNAME","Exponential.tailup"),
#    addfunccol = "afvArea")
fitRE1BLUP <- BLUP(fitSpRE1)
str(fitRE1BLUP)
fitRE1BLUP$Mean

}
