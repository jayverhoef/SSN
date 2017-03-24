\name{EmpiricalSemivariogram}
\alias{EmpiricalSemivariogram}

\title{
Empirical Semivariogram Based on Euclidean Distance
}
\description{
\command{EmpiricalSemivariogram} computes the empirical semivariogram from
the data based on Euclidean distance. 
}
\usage{
EmpiricalSemivariogram(ssn.object, varName, nlag = 20, directions = c(0, 45, 90, 135), 
    tolerance = 22.5, inc = 0, maxlag = 1e+32, nlagcutoff = 1, EmpVarMeth = "MethMoment")
}

\arguments{
  \item{ssn.object}{
    an object of class \link{SpatialStreamNetwork-class} or \link{influenceSSN-class}
}
  \item{varName}{
    a response or residual variable name in the data.frame of observed data in 
    the SpatialStreamNetwork or influenceSSN object.
}
  \item{nlag}{
    the number of lag bins to create, by direction if directions are specified.  
	The distance between endpoints that
    define a bin will have equal lengths for all bins.  The bin sizes are then
    determined from the minimum lag in the data, and the specification of maxlag.
}
  \item{directions}{
	directions in degrees clockwise from north that allow lag binning to be 
	directional.  Default is c(0, 45, 90, 135).  Values should be between 0
	and 180, as there is radial symmetry in orientation between two points.
}
  \item{tolerance}{
	the angle on either side of the directions to determine if a pair of points
	falls in that direction class.  Note, a pair of points may be in more than
	one lag bin if tolerances for different directions overlap.
}
  \item{inc}{
	the distance increment for each bin class.  Default is 0, in which case
	maxlag and nclasses determine the distance increments. 
}
  \item{maxlag}{
    the maximum lag distance to consider when binning pairs of locations by the
    hydrologic distance that separates them.  If the specified maxlag is larger
	than the maximum distance among pairs of points, then maxlag is set to the 
	maximum distance among pairs.  If inc is greater than 0, then maxlag is
	disregarded.
}
  \item{nlagcutoff}{
    the minimum number of pairs needed to estimate the semivariance for a bin. 
    If the sample size is less than this value, the semivariance for the bin is
    not calculated.
}
  \item{EmpVarMeth}{
	method for computing semivariances.  The default is "MethMoment", the classical 
	method of moments, which is just the average difference-squared within 
	bin classes. "Covariance" computes covariance rather than semivariance, but may
	be more biased because it subtracts off the simple mean of the response variable.
	"RobustMedian" and "RobustMean" are robust estimators proposed by Cressie and Hawkins 
	(1980). If v is a vector of all pairwise square-roots of absolute differences within 
	a bin class, then RobustMedian computes median(v)^4/.457. "RobustMean" computes 
	mean(v)^4/(.457 + .494/length(v)).
}
}

\value{
A list of six vectors.  The lengths of all vectors are equal, which
is equal to nlag*(number of directions) - (any missing lags due to nlagcutoff). 
  \item{distance}{
   the mean Euclidean distance separating pairs of sites used 
   to calculate the semivariance for each bin
}
  \item{gamma}{
   the estimated semivariance for each bin, based on EmpVarMeth
}
  \item{np}{
    the number of pairs of sites used to calculate the semivariance for each bin 
}
  \item{azimuth}{
	the azimuth, equivalent to the direction, used for the bin class
}
  \item{hx}{
	the x-coordinate of the center of the bin lag.
}
  \item{hy}{
	the y-coordinate of the center of the bin lag.
}
}

\author{
Jay Ver Hoef \email{support@SpatialStreamNetworks.com}
}

\examples{

library(SSN)
# NOT RUN 
# mf04 <- importSSN(system.file("lsndata/MiddleFork04.ssn", 
#	        package = "SSN"), o.write = TRUE)
# use SpatialStreamNetwork object mf04 that was already created
data(mf04)
#make sure mf04 has the correct path, will vary for each users installation
mf04 <- updatePath(mf04, system.file("lsndata/MiddleFork04.ssn", package = "SSN"))
    
# Compute the empirical semivariogram for the raw data. 
# the number of pairs used to estimate the semivariance
EVout <- EmpiricalSemivariogram(mf04, "Summer_mn", directions = 0, tolerance = 180,
    nlag = 10)
# Plot it and set the point size relative to the number of pairs used to estimate
# the semivariance
plot(EVout$distance, EVout$gamma, pch = 19, cex = EVout$np/100, ylim = c(0,6),
    ylab = "Gamma", xlab = "Distance", col = "blue", 
    main = "Empirical Semivariogram - Raw Data")

# generate and plot an empirical semivariogram based on model residuals
data(modelFits)
#make sure fitSP has the correct path, will vary for each users installation
fitSp$ssn@path <- system.file("lsndata/MiddleFork04.ssn", package = "SSN")
fitSpRes <- residuals(fitSp)
names(getSSNdata.frame(fitSpRes))
EVresid <- EmpiricalSemivariogram(fitSpRes, "_resid_", directions = 0, 
    tolerance = 180, nlag = 7,	maxlag = 15000,)
plot(EVresid$distance, EVresid$gamma, pch = 19, cex = EVresid$np/50, ylim = c(0,6),
    ylab = "Gamma", xlab = "Distance", main = "Empirical Semivariogram - Residuals")

}    
