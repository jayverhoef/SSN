\name{glmssn}
\alias{glmssn}
\title{
Fitting Generalized Linear Models for Spatial Stream Networks
}
\description{
This function works on objects of class \code{\link[=SpatialStreamNetwork-class]{SpatialStreamNetwork}} to fit generalized
linear models with spatially autocorrelated errors using normal likelihood
methods (including REML) and quasi-likelihood for Poisson and Binomial families.
The spatial formulation is described in Ver Hoef and Peterson (2010) and Peterson
and Ver Hoef (2010).
}
\usage{
glmssn(formula, ssn.object, family = "Gaussian", CorModels =
  c("Exponential.tailup","Exponential.taildown","Exponential.Euclid"),
  use.nugget = TRUE, use.anisotropy = FALSE, addfunccol = NULL, trialscol = NULL, 
  EstMeth = "REML", useTailDownWeight = FALSE, trans.power = NULL,trans.shift = 0,
  control = list(max.range.factor = 4, trunc.pseudo = NULL,
  maxiter.pseudo = 20, beta.converge = 1e-05))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
    an object of class "formula" (or one that can be coerced to that class):
    a symbolic description of the model to be fitted. The details of model
    specification are given under 'Details'.
}
  \item{ssn.object}{
    an object of class \code{\link[=SpatialStreamNetwork-class]{SpatialStreamNetwork}}, representing a spatial
    stream network. This contains the variables used in the model.
    }
  \item{family}{
    the error distribution and link function to be used in the
    model. This is a character string that is either "Gaussian", "Poisson", or
    "Binomial."
  }
  \item{CorModels}{
    a vector of spatial autocorrelation models for stream networks.  The
    individual models should be of different "types" tail-up, tail-down,
    Euclidean, or NULL for a non-spatial model. The tailup models include:
    "Exponential.tailup" (default), "LinearSill.tailup",
    "Spherical.tailup", "Mariah.tailup" "Epanech.tailup"; tail-down models
		include: "Exponential.taildown" (default), "LinearSill.taildown",
		"Spherical.taildown", "Mariah.taildown", "Epanech.taildown"; 
		Euclidean distance models include: "Spherical.Euclid",
    "Gaussian.Euclid", "Exponential.Euclid" (default), "Cauchy.Euclid". 
		The first 4 tailup and taildown models are described in Ver Hoef and
		Peterson (2010), and the "Epanech" models are described in Garreta, 
		Monestiez, and Ver Hoef (2010), and
    the 4 Euclidean distance models are standard spatial covariance
    models. If this is NULL, then use.nugget = TRUE will impose
    independence between observations, or a classical regression
    analysis non-spatial model. Basic random effects can be included in
    the model here also. See examples below.
  }
  \item{use.nugget}{
    add a nugget effect, default is TRUE.  This can be thought of as a
    variance component for independent errors, adding a variance
    component only along the diagonal of the covariance matrix.
  }
  \item{use.anisotropy}{
    use anistropy for the Euclidean distance based spatial
    model in CorModels
  }
  \item{addfunccol}{
    the name of the variable in the \code{\link[=SpatialStreamNetwork-class]{SpatialStreamNetwork}} object that is
    used to define spatial weights. For the tailup models, weights need to be used for branching.  This is an additive
    function and is described in Ver Hoef and Peterson (2010). See example below.
  }
  \item{trialscol}{
    name of the variable in the \code{\link[=SpatialStreamNetwork-class]{SpatialStreamNetwork}} object that contains
    the sample size when a binomial distribution is used.  If NULL, a sample size
    of 1 is assumed, and the response variable must be binary (0 or 1).
  }
  \item{EstMeth}{
    Estimation method; either "ML" for maximum likelihood, or "REML" for restricted
    maximum likelihood (default).
  }
  \item{useTailDownWeight}{
    use stream segment weighting in the tail-down model?  Default is FALSE.
		Weighting is same as for tail-up models, based on an additive function.
  }
  \item{trans.power}{
    power transformation for the response variable in case of Gaussian
    data.  It must be between 0 and 0.5, and if 0, a natural log is used.
  }
  \item{trans.shift}{
    a shift (addition or subtraction) applied to the response variable prior to
   the power tranformation
 }
 \item{control}{
   a list of control parameters, consisting of four items: 1) max.range.factor; this
   sets the maximum range as a function of the maximum distance among observed 
   data locations, 2) trunc.pseudo; this sets a truncation value for pseudo-data for 
   the quasi-models (family binomial and poisson).  Because the data are modeled on 
   a log or logit scale, exponentiation can cause numerical overflows, so this sets
   an upper bound, 3) maxiter.pseudo; this sets the maximum number of iterations
   when creating pseudo data for quasi-models. 4)beta.converge; this sets convergence criteria on fixed effect estimates.  When all changes in the fixed effect estimates are less than beta.converge during an iteratively reweighted least squares update, then iteration stops.  The default setting for control
   is control = list(max.range.factor = 4, trunc.pseudo = NULL, maxiter.pseudo = 20, beta.converge = 1e-5)
   }
}
\details{
Models for glmssn are specified symbolically, similar to \code{\link{lm}}
and other models in R.  A typical model has the form response ~ terms
where response is the (numeric) response vector and terms is a series of
fixed effect linear predictors for the response.  A terms specification of
the form first + second indicates all the terms in first together with
all the terms in second with duplicates removed.  A specification of the
form first:second indicates the set of terms obtained by taking the
interactions of all terms in first with all terms in second.  The
specification first*second indicates the cross of first and second.
This is the same as first + second + first:second. See
\code{\link{model.matrix}} for further details.  The terms in the
formula will be re-ordered so that main effects come first, followed by
the interactions, all second-order, all third-order and so on.  A
formula has an implied intercept term. To remove this use either y ~ x -
1 or y ~ 0 + x.  See \code{\link{formula}} for more details of allowed
formulae.

The spatial formulation is described in Ver Hoef and Peterson (2010) and Peterson
and Ver Hoef (2010).
}
\value{
  \item{args}{
   Information on arguments used in the function call to \code{\link{glmssn}}
   }
  \item{ssn.object}{
   a copy of the input object of class \code{SpatialStreamNetwork}, so that the
   model fit is directly tied to an \code{SpatialStreamNetwork} object
   }
  \item{sampinfo}{
   sample information
   }
  \item{estimates}{
   Estimates of the covariance parameters
   }
  \item{optimOutput}{
   Output from last call to optim to enable the user
    to check for correct convergence
    }

  \command{glmssn} returns an object of class "glmssn".
  This is a list of 5 objects, with the following structure:

  \preformatted{
    outpt <- list(
    args = list( ## stores all arguments used in function call
    formula = formula,
    zcol = dataXY.out$respvecs$response.col, # response column
    family = family,
    CorModels = CorModels,
    useTailDownWeights = useTailDownWeights,
    use.nugget = use.nugget,
    use.anisotropy = use.anisotropy,
    addfunccol = addfunccol,
    trialscol = trialscol,
    EstMeth = EstMeth,
    trans.power = trans.power,
    trans.shift = trans.shift
    ),

    ssn.object = ssn.object, # input object of class "SpatialStreamNetwork"

    sampinfo = list( # sample information
    # indicator vector for non-missing response values
    ind.obs = ind[order(data[,"pid"])], 
    sample.size = n.all, # total number of records in the data frame
    # number of records with non-missing response values
    obs.sample.size = n.allxy, 
    missing.sample.size = n.all - n.allxy, # number of missing response values
    rankX = p, # rank of X
    # vector of the response variable
    z = zt[order(dataXY.out$datasets$data2[,"pid"])], 
    X = X2[order(dataXY.out$datasets$data2[,"pid"]),], # design matrix
    effnames = dataXY.out$Xmats$effnames,
    setzero = dataXY.out$indvecs$setzero,
    setNA = dataXY.out$indvecs$setNA,
    setNA2 = dataXY.out$indvecs$setNA2,
    cutX1toX2 = dataXY.out$indvecs$cutX1toX2,
    StdXDataFrame = dataXY.out$Xmats$StdXDataFrame
    ),

    estimates = list(
    theta=parmest, # estimated covariance parameters
    # estimated covariance matrix
    V = V[order(dataXY.out$datasets$data2[,"pid"]),
    order(dataXY.out$datasets$data2[,"pid"])],
    # inverse of estimated covariance matrix
    Vi = Vi[order(dataXY.out$datasets$data2[,"pid"]), 
    order(dataXY.out$datasets$data2[,"pid"])],
    betahat = b.hat, # estimated fixed effects
    covb = covb, # estimated covariance matrix of estimated fixed effects
    # inverse of estimated covariance matrix of estimated fixed effects
    covbi = covbi, 
    m2LL = m2LL # -2 times log-likelihood
    ),

    optimOutput=parmest.out
    )
  }
}

\references{
\cite{Garreta, V., Monestiez, P. and Ver Hoef, J.M. (2010) Spatial modelling and prediction on river networks: up model, down model, or hybrid? \emph{Environmetrics} \bold{21(5)},
    439--456.}

\cite{Peterson, E.E. and Ver Hoef, J.M. (2010) A mixed-model moving-average approach
    to geostatistical modeling in stream networks. \emph{Ecology} \bold{91(3)},
    644--651.}

\cite{Ver Hoef, J.M. and Peterson, E.E. (2010) A moving average approach for spatial
    statistical models of stream networks (with discussion).
    \emph{Journal of the American Statistical Association} \bold{105}, 6--18.
    DOI: 10.1198/jasa.2009.ap08248.  Rejoinder pgs. 22--24.}

}

\author{
Jay Ver Hoef \email{support@SpatialStreamNetworks.com}
}

\examples{

library(SSN)
#for examples, copy MiddleFork04.ssn directory to R's temporary directory
copyLSN2temp()
# NOT RUN
# Create a SpatialStreamNetork object that also contains prediction sites
#mf04p <- importSSN(paste0(tempdir(),'/MiddleFork04.ssn'), 
#  predpts = "pred1km", o.write = TRUE)

#use mf04p SpatialStreamNetwork object, already created
data(mf04p)
#for examples only, make sure mf04p has the correct path
#if you use importSSN(), path will be correct
mf04p <- updatePath(mf04p, paste0(tempdir(),'/MiddleFork04.ssn'))

# The models take a little time to fit, so they are NOT RUN
# Uncomment the code to run them
# Alternatively, you can load the fitted models first to look at results
data(modelFits)

## Non-spatial model
# fitNS <- glmssn(Summer_mn ~ ELEV_DEM + netID,
#    ssn.object = mf04p, CorModels = NULL, 
#    EstMeth = "REML", family = "Gaussian")
#for examples only, make sure fitNS has the correct path
#if you use importSSN(), path will be correct
fitNS$ssn.object <- updatePath(fitNS$ssn.object, 
	paste0(tempdir(),'/MiddleFork04.ssn'))
summary(binSp)

summary(fitNS)

## Random effect model using STREAMNAME as our random effect
#fitRE <- glmssn(Summer_mn ~ ELEV_DEM + netID,
#    ssn.object = mf04p, EstMeth = "REML", family = "Gaussian",
#    CorModels = c("STREAMNAME"))
#for examples only, make sure fitRE has the correct path
#if you use importSSN(), path will be correct
fitRE$ssn.object <- updatePath(fitRE$ssn.object, 
	paste0(tempdir(),'/MiddleFork04.ssn'))
summary(fitRE)
## random effects details
fitREBLUP <- BLUP(fitRE)
str(fitREBLUP)
fitREBLUP$Mean

## Basic spatial model with a random effect
#fitSpRE1 <- glmssn(Summer_mn ~ ELEV_DEM + netID,
#    ssn.object = mf04p, EstMeth = "REML", family = "Gaussian",
#    CorModels = c("STREAMNAME","Exponential.Euclid"))
#for examples only, make sure fitSpRE1 has the correct path
#if you use importSSN(), path will be correct
fitSpRE1$ssn.object <- updatePath(fitSpRE1$ssn.object, 
	paste0(tempdir(),'/MiddleFork04.ssn'))
summary(fitSpRE1)

## Spatial stream tail-up model with a random effect
#fitSpRE2 <- glmssn(Summer_mn ~ ELEV_DEM + netID,
#    ssn.object = mf04p, EstMeth = "REML", family = "Gaussian",
#    CorModels = c("STREAMNAME","Exponential.tailup"),
#    addfunccol = "afvArea")
#for examples only, make sure fitSpRE2 has the correct path
#if you use importSSN(), path will be correct
fitSpRE2$ssn.object <- updatePath(fitSpRE2$ssn.object, 
	paste0(tempdir(),'/MiddleFork04.ssn'))
summary(fitSpRE2)

## 3 component spatial model
#fitSp <- glmssn(Summer_mn ~ ELEV_DEM + netID,
#    ssn.object = mf04p, EstMeth = "REML", family = "Gaussian",
#    CorModels = c("Exponential.tailup","Exponential.taildown",
#    "Exponential.Euclid"), addfunccol = "afvArea")
#for examples only, make sure fitSp has the correct path
#if you use importSSN(), path will be correct
fitSp$ssn.object <- updatePath(fitSp$ssn.object, 
	paste0(tempdir(),'/MiddleFork04.ssn'))


## Summarise last model
summary(fitSp)

## AIC for last model
AIC(fitSp)

## Generalised R-squared for last model
GR2(fitSp)

## Look at variance components in more detail
covparms(fitSp)
varcomp(fitSp)

## Compare models
InfoCritCompare(list(fitNS, fitRE, fitSpRE1, fitSpRE2, fitSp))

## Fit a model to binary data
#binSp <- glmssn(MaxOver20 ~ ELEV_DEM + SLOPE, mf04p,
#  CorModels = c("Mariah.tailup", "Spherical.taildown"),
#  family = "binomial", addfunccol = "afvArea")
#for examples only, make sure binSp has the correct path
#if you use importSSN(), path will be correct
binSp$ssn.object <- updatePath(binSp$ssn.object, 
	paste0(tempdir(),'/MiddleFork04.ssn'))
summary(binSp)

## Fit a model to count data
#poiSp <- glmssn(C16 ~ ELEV_DEM + SLOPE, mf04p,
#  CorModels = c("LinearSill.tailup", "LinearSill.taildown"),
#  family = "poisson", addfunccol = "afvArea")
#for examples only, make sure poiSp has the correct path
#if you use importSSN(), path will be correct
poiSp$ssn.object <- updatePath(poiSp$ssn.object, 
	paste0(tempdir(),'/MiddleFork04.ssn'))
summary(poiSp)

}

