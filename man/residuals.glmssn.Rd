\name{residuals.glmssn}
\alias{residuals.glmssn}
\title{
Compute Model Residuals for glmssn Objects
}
\description{
  \code{residuals.glmssn} is a generic function that has been modified
  for \code{glmssn} objects. It produces residuals from glmssn spatial models.

}
\usage{
\method{residuals}{glmssn}(object, cross.validation=FALSE, ...)
}
\arguments{
  \item{object}{
    an object of class \link{glmssn}
  }
  \item{cross.validation}{
	logical value indicating whether leave-one-out cross-validation residuals
	will be computed. The default is FALSE. Setting \code{cross.validation} to
	TRUE may increase processing times for large datasets.
  }
  \item{...}{Other arguments}

}
\details{
  When using \code{residual(x)} on a glmssn object, the data for which the model was fit
  is contained in the obspoints slot @SSNPoints@point.data.  This data frame contains the
  response variable for the model, so it is appended with the following columns,
  \describe{
    \item{obsval}{
      The response value used for fitting the model
    }
		\item{_fit_}{
			For a model z = Xb + e, where X is a design matrix for fixed effects and e
			contains all random components, then the fit is Xb, where b contains the
			estimated fixed effects parameters.
		}
    \item{_resid_}{
      The raw residuals.  The observed response value minus the fitted value
      using only fixed effect estimates (no random effects are included).
    }
    \item{_resid.stand_}{
      Standardized residuals, calculated by dividing the raw residuals by the
	corresponding estimated standard errors
    }
    \item{_resid.student_}{
      Studentized residuals.  From a model z = Xb + e, we can create uncorrelated
      data by taking a model Cz = CXb + Ce, where var(e) = sV, C is the square
      root inverse of V, and s is an overall variance parameter.  Under such a
      model, the hat matrix is H = CX*inv(X'(C'C)X)*X'C'.  Then, the variance of
      a residual is s(1-H[i,i]), and so the studentized residual is
      r[i]/sqrt(s(1-H[i,i]), where r[i] is the ith raw residual.
    }
    \item{_leverage_}{
      Leverage.  H[i,i] as described for Studentized residuals.
    }
    \item{_CooksD_}{
      Cook's D, using the method of creating uncorrelated data as for
      Studentized residuals, and then applying Cook's D.
    }
    \item{_resid.crossv_}{
      Cross-validation residuals, obtained from leave-one-out-at-a-time and
      taking the difference between the observed response value and that
      predicted after removing it. Only computed if \code{cross.validation} was set to TRUE.
    }
    \item{_CrossValPred_}{
      The leave-one-out cross-validation predictions. Only computed if \code{cross.validation}
      is set to TRUE.
    }
    \item{_CrossValStdErr_}{
      Estimated standard errors for the leave-one-out cross-validation predictions.
      Only computed if \code{cross.validation} is set to TRUE.
    }
  }
}
\value{
The returned object is of class \code{\link{influenceSSN-class}}. It similar to
a \link{glmssn-class}object; the main difference is that additional columns
(described in the details section) have been added to the observed points data.frame.
}

\author{
  Jay Ver Hoef \email{support@SpatialStreamNetworks.com}
}

\examples{

	library(SSN)
	data(modelFits)
	#make sure fitSp has the correct path, will vary for each users installation
	fitSp$ssn.object@path <- system.file("lsndata/MiddleFork04.ssn", package = "SSN")
	names(fitSp)
	names(fitSp$ssn.object)

  resids <- residuals(fitSp)
  class(resids)
  names(resids)
  plot(resids)
  hist(resids, xlab = "Raw Residuals")
  qqnorm(resids)

  resids.df <- getSSNdata.frame(resids)
  plot(resids.df[,"_resid_"], ylab = "Raw Residuals")

}
