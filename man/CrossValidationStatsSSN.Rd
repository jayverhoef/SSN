\name{CrossValidationStatsSSN}
\alias{CrossValidationStatsSSN}
\title{
Compute Summary Statistics on Crossvalidation Values for glmssn Objects
}
\description{
  \code{CrossValidationStatsSSN} operates on glmssn objects and uses the
  \code{CrossValidationSSN} function to create a data.frame of crossvalidation
  predictions and standard errors.  Then it computes summary statistics such
  as bias and confidence interval coverage based on cross-validation.

}
\usage{
  CrossValidationStatsSSN(object)
}
\arguments{
  \item{object}{
    an object of class 'glmssn'
  }

}
\details{
  This function uses the
  \code{CrossValidationSSN} function to create a data.frame of crossvalidation
  predictions and standard errors.  Then it computes summary statistics on bias,
  root mean-squared prediction errors (RMSPE), and confidence interval coverage
  based on cross-validation. Output is a data.frame with with a single entry 
  for the columns as describe below.  In the descriptions, obs is an observed
  data value, pred is its prediction using crossvalidation, and predSE is the 
  prediction standard error using crossvalidation.
  \describe{
    \item{bias}{
      Bias, computed as mean(obs - pred).
    }
    \item{std.bias}{
      Standardized bias, computed as mean((obs - pred)/predSE).
    }
    \item{RMSPE}{
      Root mean-squared prediction error, computed as
      sqrt(mean((obs - pred)^2))
    }
    \item{RAV}{
      Root average variance, computed as sqrt(mean(predSE^2)).  If the prediction
      standard errors are being estimated well, this should be close to RMSPE.
    }
    \item{std.MSPE}{
      standardized mean-squared prediction error, computed as
      mean(((obs - pred)/predSE)^2). If the prediction standard errors are being
      estimated well, this should be close to 1.
    }
    \item{cov.80}{
      The proportion of times that obs was within the prediction interval formed
      from pred +- qt(.9, df)*predSE, where qt is the quantile t function, and
      df is the number of degrees of freedom.  If there is little bias and the 
      prediction standard errors are being estimated well, this should be 
      close to 0.8 for large sample sizes. 
    }
    \item{cov.90}{
      The proportion of times that obs was within the prediction interval formed
      from pred +- qt(.95, df)*predSE, where qt is the quantile t function, and
      df is the number of degrees of freedom.  If there is little bias and the 
      prediction standard errors are being estimated well, this should be 
      close to 0.9 for large sample sizes. 
    }
    \item{cov.95}{
      The proportion of times that obs was within the prediction interval formed
      from pred +- qt(.975, df)*predSE, where qt is the quantile t function, and
      df is the number of degrees of freedom.  If there is little bias and the 
      prediction standard errors are being estimated well, this should be 
      close to 0.95 for large sample sizes. 
    }
  }

}
\value{
  Output is a data.frame with with a single entry for the columns as listed
  above.
}

\author{
  Jay Ver Hoef \email{support@SpatialStreamNetworks.com}
}

\seealso{
  \code{\link{InfoCritCompare}}, \code{\link{glmssn}}, \code{\link{CrossValidationSSN}}
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

## 3 component spatial model
#fitSp <- glmssn(Summer_mn ~ ELEV_DEM + netID,
#    ssn.object = mf04, EstMeth = "REML", family = "Gaussian",
#    CorModels = c("Exponential.tailup","Exponential.taildown",
#    "Exponential.Euclid"), addfunccol = "afvArea")
CrossValidationStatsSSN(fitSp)

}
