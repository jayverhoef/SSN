\name{InfoCritCompare}
\alias{InfoCritCompare}
\title{
Compare \code{glmssn} Information Criteria
}
\description{
\code{\link{InfoCritCompare}} displays important model criteria for each object
of class \link{glmssn} object in the model list.
}
\usage{
InfoCritCompare(model.list)
}
\arguments{
  \item{model.list}{
  a list of fitted \link{glmssn-class} model objects in the form 
  \code{list(model1, model2, ...)}
}
}
\details{
  \code{\link{InfoCritCompare}} displays important model criteria that can be
  used to compare and select spatial statistical models. For instance, spatial
  models can be compared with non-spatial models, other spatial models, or both.
  

}
\value{
  \code{\link{InfoCritCompare}} returns a data.frame of the model criteria for
  each specified \link{glmssn-class} object. These are useful for comparing and
  selecting models. The columns in the data.frame are described below. In the 
  description below 'obs' is an observed data value, 'pred' is its prediction
  using cross-validation, and 'predSE' is the prediction standard error using
  cross-validation.
\describe{
	\item{formula}{
	 model formula
    }
    \item{EstMethod}{
	  estimation method, either maximum likelihood (ML) or restricted
	  maximum likelihood (REML)
    }
    \item{Variance_Components}{
	 names of the variance components, including the autocovariance model names, 
	 the nugget effect, and the random effects.
    }
    \item{neg2Log}{
		-2 log-likelihood. Note that the neg2LogL is only returned if the
		Gaussian distribution (default) was specified when creating the \code{glmssn}
		object.
    }
    \item{AIC}{
		Akaike Information Criteria (AIC). Note that AIC is only returned if the
		Gaussian distribution (default) was specified when creating the \code{glmssn}
		object.
    }
    \item{bias}{
      bias, computed as mean(obs - pred).
    }
    \item{std.bias}{
      standardized bias, computed as mean((obs - pred)/predSE).
    }
    \item{RMSPE}{
      root mean-squared prediction error, computed as
      sqrt(mean((obs - pred)^2))
    }
    \item{RAV}{
      root average variance, computed as sqrt(mean(predSE^2)).  If the prediction
      standard errors are being estimated well, this should be close to RMSPE.
    }
    \item{std.MSPE}{
      standardized mean-squared prediction error, computed as
      mean(((obs - pred)/predSE)^2). If the prediction standard errors are being
      estimated well, this should be close to 1.
    }
    \item{cov.80}{
      the proportion of times that the observed value was within the prediction 
      interval formed from pred +- qt(.9, df)*predSE, where qt is the quantile 
      t function, and df is the number of degrees of freedom.  If there is little 
      bias and the prediction standard errors are being estimated well, this 
      should be close to 0.8 for large sample sizes.
    }
    \item{cov.90}{
      the proportion of times that observed value was within the prediction 
      interval formed from pred +- qt(.95, df)*predSE, where qt is the quantile 
      t function, and df is the number of degrees of freedom.  If there is little 
      bias and the prediction standard errors are being estimated well, this 
      should be close to 0.9 for large sample sizes.
    }
    \item{cov.95}{
      the proportion of times that the observed value was within the prediction 
      interval formed from pred +- qt(.975, df)*predSE, where qt is the quantile 
      t function, and df is the number of degrees of freedom.  If there is little 
      bias and the prediction standard errors are being estimated well, this
      should be close to 0.95 for large sample sizes.
    }
}
}

\author{
Jay Ver Hoef \email{support@SpatialStreamNetworks.com}
}

\seealso{
	\code{\link{glmssn}}, \code{\link{summary.glmssn}}, \code{\link{AIC}},
	\code{\link{CrossValidationStatsSSN}}
}
\examples{

	library(SSN)
	data(modelFits)
	#for examples only, make sure all models have the correct path
	#if you use importSSN(), path will be correct
	fitNS$ssn.object <- updatePath(fitNS$ssn.object, 
		paste0(tempdir(),'/MiddleFork04.ssn'))
	fitRE$ssn.object <- updatePath(fitRE$ssn.object, 
		paste0(tempdir(),'/MiddleFork04.ssn'))
	fitSp$ssn.object <- updatePath(fitSp$ssn.object, 
		paste0(tempdir(),'/MiddleFork04.ssn'))
	fitSpRE1$ssn.object <- updatePath(fitSpRE1$ssn.object, 
		paste0(tempdir(),'/MiddleFork04.ssn'))
	fitSpRE2$ssn.object <- updatePath(fitSpRE2$ssn.object, 
		paste0(tempdir(),'/MiddleFork04.ssn'))

  compare.models <- InfoCritCompare(list(fitNS, fitRE, fitSp, fitSpRE1, fitSpRE2))
  
  # Examine the model criteria
  compare.models

  # Compare the AIC values for all models with random effects
  compare.models[c(2,4,5),c("Variance_Components","AIC")]
  
  # Compare the RMSPE for the spatial models
  compare.models[c(3,4,5),c("Variance_Components","RMSPE")]
  
  # Compare the RMSPE between spatial and non-spatial models
  compare.models[c(1,3),c("formula","Variance_Components", "RMSPE")]

}
