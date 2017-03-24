
\name{SimulateOnSSN}
\alias{SimulateOnSSN}
\title{Simulating Data on Spatial Stream Networks}

\description{
This function works on objects of class
\code{\link[=SpatialStreamNetwork-class]{SpatialStreamNetwork}} to simulate
data with spatially autocorrelated errors from models as described in Ver Hoef
and Peterson (2010) and Peterson and Ver Hoef (2010). It works with simulated or
real stream networks. It can simulate from Gaussian (normal), Poisson and binomial
distributions.
}

\usage{
SimulateOnSSN(ssn.object, ObsSimDF, PredSimDF = NULL, PredID = NULL,
  formula, coefficients,
  CorModels = c("Exponential.tailup", "Exponential.taildown", "Exponential.Euclid"),
  use.nugget = TRUE, use.anisotropy = FALSE,
  CorParms = c(1, 10000, 1, 10000, 1, 10000, 0.1),
  addfunccol = NULL, useTailDownWeight = FALSE, family = "Gaussian", mean.only=FALSE)
}

\arguments{
  \item{ssn.object}{
   an object of class \code{\link[=SpatialStreamNetwork-class]{SpatialStreamNetwork}}
}
  \item{ObsSimDF}{
   a data frame used to replace the existing observed sites data frame in \code{ssn.object}.
   It is safest to first extract the point.data data.frame from \code{ssn.object}, then add
   covariate values to the extracted data.frame. See the examples section.
}
  \item{PredSimDF}{
   a data frame used to replace the existing prediction site data frame in \code{ssn.object}
   It is safest to first extract the point.data data.frame from \code{ssn.object},
   then add covariate values to the extracted data frame. See the examples section.
   The covariate names should match those from \code{ObsSimDF}.
}
  \item{PredID}{
   a string representing the ID (name) of the prediction slot in the ssn.object.
   The ith name is accessed using the call \code{ssn.object@predpoints@ID[i]}.
}
  \item{formula}{
   a one-sided formula to the right of, and including, the ~. This is similar to
   linear model formula but works in reverse. It will create a design matrix based on
   the formula and covariates in the \code{ObsSimDF} and \code{PredSimDF}.
}
  \item{coefficients}{
   a vector of numeric values representing the coefficients. The \code{formula}
   creates the design matrix, and these coefficients are multiplied by the columns
   in the design matrix. If the design matrix is X, and coefficients are
   beta, then the mean values are created as X \%*\% beta.  Note that this presumes
   some knowledge about how R will create design matrices from formulas.
}
  \item{CorModels}{
   a character vector of spatial autocorrelation model names for stream networks.
   The individual models should be of different "types".  It can be "NULL"
   for a non-spatial model, or it can contain any of the tailup models:
   "Exponential.tailup" (default), "LinearSill.tailup",
   "Spherical.tailup", "Mariah.tailup", and/or one of the taildown
   models: "Exponential.taildown" (default), "LinearSill.taildown",
   "Spherical.taildown", "Mariah.taildown", or one of the Euclidean
   distance models: "Spherical.Euclid", "Gaussian.Euclid",
   "Exponential.Euclid" (default), "Cauchy.Euclid".  The 4 tailup and
   taildown models are described in Ver Hoef and Peterson (2010) and
   the 4 Euclidean distance models are standard spatial autocorrelation
   models. If this is NULL, then use.nugget = TRUE will impose
   independence between observations, or a classical non-spatial linear model.
}
  \item{use.nugget}{
    logical. Add a nugget effect, default is TRUE.  This can be thought of as a
    variance component for independent errors, adding a variance
    component only along the diagonal of the covariance matrix.
}
  \item{use.anisotropy}{
    logical. Use anistropy for the Euclidean distance based spatial
    model in CorModels.  Not implemented at the current time.
}
  \item{CorParms}{
   a vector of numeric covariance parameters. Each of the CorModels will generally
   have two parameters, a partial sill and a range (in that order, and in the order
   as specified by CorModels). If use.nugget = TRUE, then a final CorParms parameter
   should be added for the nugget effect.
}
  \item{addfunccol}{
   for the tailup models, weights are need to be used to account for dendritic
   branching in the network. This is achieved using an additive function and is
   described in Ver Hoef and Peterson (2010). The name of the variable in the
   ssn.object that is to be used to define weights should be given here.
   See example below.
}
  \item{useTailDownWeight}{
   Use weighting in the tail-down models in the same way as for tail-up models.
	 Logical that defaults to FALSE.
 }
  \item{family}{
   the error distribution and link function to be used in the model. This is a
   character string that is either "Gaussian" (default), "Poisson", or "Binomial."
 }
 \item{mean.only}{
   Logical that defaults to FALSE.
   }
}
\details{
Models are specified symbolically in a manner similar to \code{\link{lm}}
and other model-fitting functions in R, but here the formula is right-handed (e.g.
~ x1 + x2 + x3, where x1, x2, x3 are the 'terms'). If the formula is specified
as ~ terms, data will be simulated as Sim_Values ~ terms, where Sim_Values is the
(numeric) response vector and terms is a series of fixed effect linear predictors
for Sim_Values.  A terms specification of the form first + second indicates all
the terms in first together with all the terms in second with duplicates removed.
A specification of the form first:second indicates the set of terms obtained by
taking the interactions of all terms in first with all terms in second. The
specification first*second indicates the cross of first and second. This is the
same as first + second + first:second. See \code{\link{model.matrix}} for additional
details. The terms in the formula are re-ordered so that main effects come first,
followed by the interactions, all second-order, all third-order and so on. A
formula has an implied intercept term. To remove this use either  ~ x -
1 or  ~ 0 + x.  See \code{\link{formula}} for more details about allowable
formulae.

The observed data data.frame used for simulating is contained in the slot
ssn.object@obspoints@SSNPoints[[1]]@point.data and can be easily accessed
using \code{\link{getSSNdata.frame}}. The function \code{\link{putSSNdata.frame}}
can be used to to put it back after it has been modified.  Likewise, the predicted
data data.frame used for simulating is contained in stored in
ssn.object@predpoints@SSNPoints[[i]]@point.data, where i is the ith prediction
data set within \code{ssn.object}; generally i = 1, but is not a limit on the number
of prediction datasets that may be included. Calls to \code{getSSNdata.frame} and
\code{putSSNdata.frame} may be used to access the prediction site data.frames as well.
}
\value{
Output from SimulateOnSSN contains three list items.
  \item{ssn.object}{
   the input SSN that now has simulated data in the observed and/or prediction
   data.frames. Within these data.frames, the simulated data have a column
   heading called "Sim_Values"
   }
  \item{FixedEffects}{
   a data.frame of the ordered column names for the design matrix that was
   created. The first column is the column name of the design matrix, and the
   second column is the coefficient used for that fixed effect for simulation.
   This can be used to ensure that the \code{coefficients} are being used
   in the way that they were intended.
   }
  \item{CorParms}{
   a data.frame of the ordered variance component model parameters. No matter
   the order of the Corparms input argument, the covariance parameters are
   applied in the follwoing order, if specifed in \code{CorParms}:
   tailup model (partial sill then range), taildown (partiall sill then range),
   Euclidean model (partial sill then range), random effects variance components
   ordered alphanumerically, and finally the nugget. This can be used to ensure
   that the \code{CorParms} are being applied in the way that they were intended.
   }
}
\references{
\cite{Peterson, E. E. and Ver Hoef, J. M. (2010) A mixed-model moving-average
approach to geostatistical modeling in stream networks. \emph{Ecology} \bold{91(3)},
644--651.}

\cite{Ver Hoef, J. M. and Peterson, E. E. (2010) A moving average approach for
spatial statistical models of stream networks (with discussion).
\emph{Journal of the American Statistical Association} \bold{105}, 6--18.
DOI: 10.1198/jasa.2009.ap08248.  Rejoinder pgs. 22 - 24.}
}
\author{
Jay Ver Hoef \email{support@SpatialStreamNetworks.com}
}

\examples{
#######################################
## example 1: Gaussian data, 2 networks
#######################################

library(SSN)
set.seed(101)
## simulate a SpatialStreamNetwork object
raw1.ssn <- createSSN(n = c(10,10),
    obsDesign = binomialDesign(c(50,50)), predDesign = binomialDesign(c(100,100)),
	importToR = TRUE, path = paste(tempdir(),"/sim1", sep = ""))
plot(raw1.ssn)

## create distance matrices, including between predicted and observed
createDistMat(raw1.ssn, "preds", o.write=TRUE, amongpred = TRUE)

## look at the column names of each of the data frames
names(raw1.ssn)

## extract the observed and predicted data frames
raw1DFobs <- getSSNdata.frame(raw1.ssn, "Obs")
raw1DFpred <- getSSNdata.frame(raw1.ssn, "preds")

## add a continuous covariate randomly
raw1DFobs[,"X1"] <- rnorm(length(raw1DFobs[,1]))
raw1DFpred[,"X1"] <- rnorm(length(raw1DFpred[,1]))

## add a categorical covariate randomly
raw1DFobs[,"F1"] <- as.factor(sample.int(3,length(raw1DFobs[,1]), replace = TRUE))
raw1DFpred[,"F1"] <- as.factor(sample.int(3,length(raw1DFpred[,1]), replace = TRUE))

## simulate Gaussian data
sim1.out <- SimulateOnSSN(raw1.ssn,
	ObsSimDF = raw1DFobs,
	PredSimDF = raw1DFpred,
	PredID = "preds",
	formula = ~ X1 + F1,
	coefficients = c(1, .5, -1, 1),
	CorModels = c("Exponential.tailup", "Exponential.taildown"),
	use.nugget = TRUE,
	use.anisotropy = FALSE,
	CorParms = c(2, 5, 2, 5, 0.1),
	addfunccol = "addfunccol")

## Columns of design matrix, coefficients argument applied to these
sim1.out$FixedEffects

## extract the ssn.object
sim1.ssn <- sim1.out$ssn.object

## extract the observed and predicted data frames, now with simulated values
sim1DFobs <- getSSNdata.frame(sim1.ssn, "Obs")
sim1DFobs[,"Sim_Values"]
sim1DFpred <- getSSNdata.frame(sim1.ssn, "preds")
sim1DFpred[,"Sim_Values"]

## plot the simulated observed values
plot(sim1.ssn, "Sim_Values")

## store simulated prediction values, and then create NAs in their place
sim1preds <- sim1DFpred[,"Sim_Values"]
sim1DFpred[,"Sim_Values"] <- NA
sim1.ssn <- putSSNdata.frame(sim1DFpred, sim1.ssn, "preds")

# NOT RUN, IT TAKES A MINUTE OR SO
## fit a model to see how well we estimate simulation parameters
#fitSimGau <- glmssn(Sim_Values ~ X1 + F1, ssn.object = sim1.ssn,
#	 CorModels = c("Exponential.tailup", "Exponential.taildown"),
#	 addfunccol = "addfunccol")
# LOAD A STORED VERSION INSTEAD
data(modelFits)
#make sure fitSimGau has the correct path, will vary for each users installation
#predictions depend on distance matrix created earlier with createDistMat function
#path of this lsn directory was created with createSSN
fitSimGau$ssn.object@path <- paste(tempdir(),"/sim1", sep = "")

summary(fitSimGau)

## make predictions
pred1.ssn <- predict(fitSimGau,"preds")
par(bg = "grey60")
plot(pred1.ssn, color.palette = terrain.colors(10))
par(bg = "white")

## compare predicted values to simulated values
pred1DF <- getSSNdata.frame(pred1.ssn, "preds")
plot(sim1preds, pred1DF[,"Sim_Values"], xlab = "True", ylab = "Predicted",
	pch = 19)

######################################
## example 2: Binomial data, 1 network
######################################
## NOT RUN takes about 10 seconds
#set.seed(102)
## simulate a SpatialStreamNetwork object
#raw2.ssn <- createSSN(n = 20,
#	obsDesign = binomialDesign(100), predDesign = binomialDesign(200),
#	importToR = TRUE, path = paste(tempdir(),"/sim2", sep = ""))
#plot(raw2.ssn)

## create distance matrices, including between predicted and observed
#createDistMat(raw2.ssn, "preds", o.write=TRUE, amongpred = TRUE)

## look at the column names of each of the data frames
#names(raw2.ssn)

## extract the observed and predicted data frames
#raw2DFobs <- getSSNdata.frame(raw2.ssn, "Obs")
#raw2DFpred <- getSSNdata.frame(raw2.ssn, "preds")

## add a continuous covariate randomly
#raw2DFobs[,"X1"] <- rnorm(length(raw2DFobs[,1]))
#raw2DFpred[,"X1"] <- rnorm(length(raw2DFpred[,1]))

## add a categorical covariate randomly
#raw2DFobs[,"F1"] <- as.factor(sample.int(3,length(raw2DFobs[,1]), replace = TRUE))
#raw2DFpred[,"F1"] <- as.factor(sample.int(3,length(raw2DFpred[,1]), replace = TRUE))

## simulate Binomial data
#sim2.out <- SimulateOnSSN(raw2.ssn,
#	ObsSimDF = raw2DFobs,
#	PredSimDF = raw2DFpred,
#	PredID = "preds",
#	formula = ~ X1 + F1,
#	coefficients = c(0, .5, -1, 1),
#	CorModels = c("Exponential.tailup", "Exponential.taildown",
#		"Exponential.Euclid"),
#	use.nugget = TRUE,
#	use.anisotropy = FALSE,
#	CorParms = c(.5, 5, .5, 5, .5, 2, 0.01),
#	addfunccol = "addfunccol",
#	family = "Binomial")

## Columns of design matrix, coefficients argument applied to these
#sim2.out$FixedEffects

## extract the ssn.object
#sim2.ssn <- sim2.out$ssn.object

## extract the observed and predicted data frames, now with simulated values
#sim2DFobs <- getSSNdata.frame(sim2.ssn, "Obs")
#sim2DFobs[,"Sim_Values"]
#sim2DFpred <- getSSNdata.frame(sim2.ssn, "preds")
#sim2DFpred[,"Sim_Values"]

## plot the simulated observed values
#plot(sim2.ssn, "Sim_Values", nclasses = 2, color.palette = c("blue","red"),
#	breaktype = "user", brks = cbind(c(-.5,.5),c(.5, 1.5)))

## store simulated prediction values, and then create NAs in their place
#sim2preds <- sim2DFpred[,"Sim_Values"]
#sim2DFpred[,"Sim_Values"] <- NA
#sim2.ssn <- putSSNdata.frame(sim2DFpred, sim2.ssn, "preds")

# NOT RUN, IT TAKES A MINUTE OR SO
## fit a model to see how well we estimate simulation parameters
#fitSimBin <- glmssn(Sim_Values ~ X1 + F1,
# 	ssn.object = sim2.ssn, EstMeth = "REML", family = "Binomial",
# 	CorModels = "Exponential.taildown",
# 	addfunccol = "addfunccol")
# LOAD A STORED VERSION INSTEAD
#data(modelFits)
#make sure fitSimBin has the correct path, will vary for each users installation
#predictions depend on distance matrix created earlier with createDistMat function
#path of this lsn directory was created with createSSN
#fitSimBin$ssn.object@path <- paste(tempdir(),"/sim2", sep = "")
#summary(fitSimBin)

## make predictions
#predSimBin <- predict(fitSimBin,"preds")
#par(bg = "grey60")
#plot(predSimBin, color.palette = terrain.colors(10))
#par(bg = "white")

## compare predicted values to simulated values
#pred2DF <- getSSNdata.frame(predSimBin, "preds")
#table(sim2preds, (pred2DF[,"Sim_Values"]>0)*1)

#####################################
## example 3: Poisson data, 1 network
#####################################
## NOT RUN Similar to Binomial Data
#set.seed(104)
## simulate a SpatialStreamNetwork object
#raw3.ssn <- createSSN(n = 20,
#	obsDesign = binomialDesign(100), predDesign = binomialDesign(200),
#	importToR = TRUE, path = paste(tempdir(),"/sim3", sep = ""))
#plot(raw3.ssn)

## create distance matrices, including between predicted and observed
#createDistMat(raw3.ssn, "preds", o.write=TRUE, amongpred = TRUE)

## look at the column names of each of the data frames
#names(raw3.ssn)

## extract the observed and predicted data frames
#raw3DFobs <- getSSNdata.frame(raw3.ssn, "Obs")
#raw3DFpred <- getSSNdata.frame(raw3.ssn, "preds")

## add a continuous covariate randomly
#raw3DFobs[,"X1"] <- rnorm(length(raw3DFobs[,1]))
#raw3DFpred[,"X1"] <- rnorm(length(raw3DFpred[,1]))
## add a categorical covariate randomly

#raw3DFobs[,"F1"] <- as.factor(sample.int(3,length(raw3DFobs[,1]), replace = TRUE))
#raw3DFpred[,"F1"] <- as.factor(sample.int(3,length(raw3DFpred[,1]), replace = TRUE))

## simulate Poisson data
#sim3.out <- SimulateOnSSN(raw3.ssn,
#	ObsSimDF = raw3DFobs,
#	PredSimDF = raw3DFpred,
#	PredID = "preds",
#	formula = ~ X1 + F1,
#	coefficients = c(1, .5, -1, 1),
#	CorModels = c("Exponential.taildown"),
#	use.nugget = TRUE,
#	use.anisotropy = FALSE,
#	CorParms = c(.5, 5, 0.01),
#	addfunccol = "addfunccol",
#	family = "Poisson")

## Columns of design matrix, coefficients argument applied to these
#sim3.out$FixedEffects

## extract the ssn.object
#sim3.ssn <- sim3.out$ssn.object

## extract the observed and predicted data frames, now with simulated values
#sim3DFobs <- getSSNdata.frame(sim3.ssn, "Obs")
#sim3DFobs[,"Sim_Values"]
#sim3DFpred <- getSSNdata.frame(sim3.ssn, "preds")
#sim3DFpred[,"Sim_Values"]

## plot the simulated observed values
#plot(sim3.ssn, "Sim_Values")

## store simulated prediction values, and then create NAs in their place
#sim3preds <- sim3DFpred[,"Sim_Values"]
#sim3DFpred[,"Sim_Values"] <- NA
#sim3.ssn <- putSSNdata.frame(sim3DFpred, sim3.ssn, "preds")

# NOT RUN, IT TAKES A MINUTE OR SO
## fit a model to see how well we estimate simulation parameters
#fitSimPoi <- glmssn(Sim_Values ~ X1 + F1,
# 	ssn.object = sim3.ssn, EstMeth = "REML", family = "Poisson",
# 	CorModels = "Exponential.taildown",
# 	addfunccol = "addfunccol")
# LOAD A STORED VERSION INSTEAD
#data(modelFits)
#make sure fitSimPoi has the correct path, will vary for each users installation
#predictions depend on distance matrix created earlier with createDistMat function
#path of this lsn directory was created with createSSN
#fitSimPoi$ssn.object@path <- paste(tempdir(),"/sim3", sep = "")
#summary(fitSimPoi)

## make predictions
#pred3.ssn <- predict(fitSimPoi,"preds")
#par(bg = "grey60")
#plot(pred3.ssn, color.palette = terrain.colors(10))
#par(bg = "white")

## compare predicted values to simulated values
#pred3DF <- getSSNdata.frame(pred3.ssn, "preds")

#plot(log(sim3preds+.1), pred3DF[,"Sim_Values"], xlab = "True", ylab = "Estimated",
#	pch = 19)

}
