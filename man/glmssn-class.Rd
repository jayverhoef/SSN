\name{glmssn-class}
\Rdversion{1.1}
\docType{class}
\alias{glmssn-class}

\title{Class "glmssn"}
\description{
a class that holds a fitted model for an object of class \code{\link[=SpatialStreamNetwork-class]{SpatialStreamNetwork}}
}
\section{Objects from the Class}{
Objects can be created by calls in the form \code{glmssn(...)}.

}
\section{List Objects}{

An object of class "glmssn" is a list of 4 objects, with the following structure:

\preformatted{
  outpt <- list(
		args = list(  ## stores all arguments used in function call
		formula,
		zcol, ## response column name
		family,
		CorModels,
		use.nugget,
		use.anisotropy,
		addfunccol,
		trialscol,
		EstMeth,
		trans.power,
		trans.shift,
		algorithm,
	),
	ssn.object = ssn.object, ## input object of class "SpatialStreamNetwork"
	sampinfo = list( ## sample information
		ind.obs, ## indicator vector for non-missing response values
		ind.RespNA, ## indicator vector for non-missing response values
		sample.size, ## Total number of records in the data frame
		obs.sample.size, ## Number of records with non-missing response values
		missing.sample.size, ## Number of missing response values
		rankX, ## rank of X
		z, ## vector of response values
		trialsvec,
		X, ## design matrix
		effnames,
		setzero,
		setNA,
		setNA2,
		cutX1toX2,
		REs,
		REmodelmatrices,
	),
	estimates = list(
		theta, ## Estimated covariance parameters
 		nugget,
		V ## Estimated covariance matrix
		Vi ## Inverse of V
		betahat ## Estimate of fixed effects
		covb ## Estimated covariance matrix of fixed effects
		covbi ## Inverse of covb
		m2LL ## -2 times log likelihood
		Warnlog ## List of warnings
	),
	loglik.surface=get("RESULT",loglik.environment),
	        optimOutput ## output from optim
	)

}
}

\section{Extends}{
Class \code{\link[=SpatialStreamNetwork-class]{SpatialStreamNetwork}}, directly.

Class \link[sp:SpatialLines-class]{SpatialLines}, by class "SpatialLinesDataFrame", distance 2.

Class \link[sp:Spatial-class]{Spatial}, by class "SpatialLinesDataFrame", distance 3.
}
\section{Methods}{
No methods defined with class "glmssn" in the signature.
}

\author{
Jay Ver Hoef \email{support@SpatialStreamNetworks.com}
}

\seealso{
  \code{\link{glmssn}}
}

\keyword{classes}

