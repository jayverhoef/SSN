\name{summary.glmssn}
\alias{summary.glmssn}
\title{
Summary - S3 Method for Class 'glmssn'
}
\description{
  \code{summary} is a generic function that produces output summaries of fitted
  models in the SSN package. In particular, the function invokes methods for
  objects of class \link{glmssn}.
}
\usage{
\method{summary}{glmssn}(object, ...)
}
\arguments{
  \item{object}{
    an object of class \code{glmssn}
  }
  \item{...}{
    other arguments passed to summary
  }
}
\details{
The \code{summary.glmssn} function summarizes the fitted model with a table of
estimates for the fixed effects and the covariance parameter estimates. It also
provides a warning log.
}
\value{
  An object with a 3-item list:

  \item{fixed.effects.estimates}{
   a data.frame with columns FactorLevel (effect & level) Estimate, std.err, 
   t.value, and prob.t (p-value) for the fixed effects
  }
  \item{covariance.parameter.estimates}{
   a list of covariance parameter estimates for each covariance model
  }
  \item{Warnings}{
  a list of warnings associated with the object
  }
}
\author{
Jay Ver Hoef \email{support@SpatialStreamNetworks.com}
}
\seealso{
  \code{\link{glmssn}},\code{link{covparms}}
}
\examples{

	library(SSN)
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
	names(fitSp)
	names(fitSp$ssn.object)

  summary(fitSp)

}
