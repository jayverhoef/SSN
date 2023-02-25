\name{print.summary.glmssn}
\alias{print.summary.glmssn}
\title{
Print summary - S3 Method for Class 'glmssn'
}
\description{
  \code{print} is a generic function that prints output summaries of fitted
  models in the SSN package. In particular, the function invokes methods for
  objects of class \link{summary.glmssn}.
}
\usage{
\method{print}{summary.glmssn}(x, digits = max(3L, getOption("digits") - 3L),
   signif.stars = getOption("show.signif.stars"), ...)
}
\arguments{
  \item{x}{
    an object of class \code{summary.glmssn}
  }
  \item{digits}{
    the number of significant digits to use when printing.
  }
  \item{signif.stars}{
    logical. If 'TRUE', 'significance stars' are printed for each coefficient.
  }
  \item{...}{
    other arguments passed to print
  }
}
\details{
The \code{print.summary.glmssn} function summarizes and prints the fitted model with a table of
estimates for the fixed effects and the covariance parameter estimates.}
\value{
  Prints the summary beginning with call and arguments used, plus:
  \item{Residuals}{
  a summary of the min, max, and quartiles of the usual residuals.
  }
  \item{Coefficients}{
   a p x 4 matrix with columns for the estimated coefficient, its standard error, 
   t-statistic and corresponding (two-sided) p-value. Aliased coefficients are omitted.
  }
  \item{Covariance Parameters}{
   a list of covariance parameter estimates for each covariance model.
  }
  \item{Residual standard error}{
  the square-root of the sum of all of the variance (partial sill) parameters.
  }
  \item{Generalized R-squared}{
  the generalized R-squared value of the fitted model
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
	data(modelFits)
	#for examples only, make sure fitSp has the correct path
	#if you use importSSN(), path will be correct
	fitSp$ssn.object <- updatePath(fitSp$ssn.object, 
		paste0(tempdir(),'/MiddleFork04.ssn'))
	names(fitSp)
	names(fitSp$ssn.object)

  print(summary(fitSp))
  #the same effect is achieved with this
  summary(fitSp)
  #or this
  fitSp
}
