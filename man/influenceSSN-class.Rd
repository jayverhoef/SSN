\name{influenceSSN-class}
\Rdversion{1.1}
\docType{class}
\alias{influenceSSN-class}

\title{Class "influenceSSN"}
\description{
  A class that extends the results of generalized linear models, \code{\link{glmssn}}
  objects, for spatial stream networks by adding influence diagnostics and cross-validation
  predictions to each observation.
}
\section{Objects from the Class}{
  Objects can be created by functions in the form \code{residual(x)},
  where \code{x} is a \link{glmssn-class} object.
}
\section{Class Structure}{
Objects of class \code{influenceSSN} contain 4 list items and have the exact same
structure as \link{glmssn-class} objects. A \code{influenceSSN} object retains
the corresponding \code{\link[=SpatialStreamNetwork-class]{SpatialStreamNetwork}}
object as the second list item. When \code{residuals(x)} is used for a glmssn object,
the data for which the model was fit is stored in point.data data.frame of the
observed points. This data.frame contains the response variable for the model,
and is appended by the following columns:
\preformatted{
  obsval            ## The response value that was used to fit the model
  _fit_
  _resid_           ## The raw residuals
  _resid.stand_     ## Standardized residuals; calculated by dividing the raw
                    ## residuals by the corresponding standard errors
  _resid.student_   ## Studentized residuals
  _leverage_        ## Leverage
  _CooksD_          ## Cook's D
  _resid.crossv_    ## Cross-validation residuals
  _CrossValPred_    ## Cross-validation predictions
  _CrossValStdErr_  ## Estimated cross-validation standard errors.
  }
}

\section{Extends}{
Class \code{"\link{glmssn}"}, directly.
}

\author{
Jay Ver Hoef \email{support@SpatialStreamNetworks.com}
}

\seealso{
  \code{\link{residuals}},\code{\link{glmssn}}
}




