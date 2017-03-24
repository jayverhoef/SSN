\name{Torgegram}
\alias{Torgegram}

\title{
Empirical Semivariogram Based on Hydrologic Distance and flow connection
}
\description{
\command{Torgegram} computes the empirical semivariogram from
the data based on hydrologic distance. The results are presented separately for
flow-connected and flow-unconnected sites.
}
\usage{
Torgegram(object, ResponseName, maxlag = NULL, nlag = 6,
	inc = 0, nlagcutoff = 15, EmpVarMeth = "MethMoment")
}

\arguments{
  \item{object}{
    an object of class \link{SpatialStreamNetwork-class} or \link{influenceSSN-class}
}
  \item{ResponseName}{
    a response or residual variable name in the data.frame of observed data in
    the SpatialStreamNetwork or influenceSSN object.
}
  \item{maxlag}{
    the maximum lag distance to consider when binning pairs of locations by the
    hydrologic distance that separates them.  The default is the median distance
    between all pairs of locations.
}
  \item{nlag}{
    the number of lag bins to create.  The distance between endpoints that
    define a bin will have equal lengths for all bins.  The bin sizes are then
    determined from the minimum lag in the data, and the specification of maxlag.
}
  \item{inc}{
    the bin distance between endpoints. It is possible to specify the bin
    distance rather than nlag.  In this case, the number of bins is determined
    by the bin distance and the distance between the mininum and maximum (maxlag)
    lag in the data
}
  \item{nlagcutoff}{
    the minimum number of pairs needed to estimate the semivariance for a bin.
    If the sample sizes is less than this value, the semivariance for the bin is
    not calculated.
  }
  \item{EmpVarMeth}{
	method for computing semivariances.  The default is "MethMoment", the classical
	method of moments, which is just the average difference-squared within
	bin classes. "Covariance" computes covariance rather than semivariance, but may
	be more biased because it subtracts off the simple mean of the response variable.
	"RobustMedian" and "RobustMean" are robust estimators proposed by Cressie and Hawkins
	(1980). If v is a vector of all pairwise square-roots of absolute differences within
	a bin class, then "RobustMedian" computes median(v)^4/.457. "RobustMean" computes
	mean(v)^4/(.457 + .494/length(v)).
}
}
\details{
The \command{Torgegram} function creates a list of hydrologic distances and
empirical semivariogram values, along with number of pairs of points in each
bin, for both flow-connected and flow-unconnected sites. Flow-connected locations
lie on the same stream network (share a common downstream junction) and water flows
from one location to the other. Flow-unconnected locations also lie on the same
stream network, but do not share flow. The output is of class
Torgegram.
}
\value{
A list of six vectors describing the semivariance values for each
bin and the hydrologic distances and number of pairs used to estimate those values.
These data are presented separately for flow-connected and flow-unconnected sites.

  \item{distance.connect}{
    the mean hydrologic distance separating pairs of flow-connected sites used
    to calculate the semivariance for each bin
}
  \item{gam.connect}{
    the mean semivariance for flow-connected sites in each bin
}
  \item{np.connect}{
    the number of pairs of flow-connected sites used to calculate the semivariance
    for each bin
}
  \item{distance.unconnect}{
    the mean hydrologic distance separating pairs of flow-connected sites used
    to calculate the semivariance for each bin
}
  \item{gam.unconnect}{
    the mean semivariance for flow-connected sites in each bin
}
  \item{np.unconnect}{
    the number of pairs of flow-connected sites used to calculate the semivariance
    for each bin
}
}

\author{
Jay Ver Hoef \email{support@SpatialStreamNetworks.com}
}

\seealso{
A generic \code{\link{plot}} operates on the object created here.
}

\examples{

	library(SSN)
	data(mf04p)
	#make sure mf04p has the correct path, will vary for each users installation
	mf04p@path <- system.file("lsndata/MiddleFork04.ssn", package = "SSN")

	ESVF <- Torgegram(mf04p, "Summer_mn")
	plot(ESVF)

	ESVF <- Torgegram(mf04p, "Summer_mn", maxlag = 20000, nlag = 10)
	plot(ESVF, sp.relationship = "fc", col = "red", main = "Flow-connected Torgegram")
	plot(ESVF, sp.relationship = "fu", min.cex = .4, max.cex = 8,
		   main = "Flow-unconnected Torgegram")
	plot(ESVF, min.cex = .4, max.cex = 8, col = c("darkgray", "black"),
		   main = "", xlab = "Stream Distance (m)")

	# generate and plot an empirical semivariogram based on model residuals
	data(modelFits)
	#make sure fitSp has the correct path, will vary for each users installation
	fitSp$ssn.object@path <- system.file("lsndata/MiddleFork04.ssn", package = "SSN")
	resids <- residuals(fitSp)
	names(resids$ssn.object)
	ESVF <- Torgegram(resids, "_resid_", maxlag = 20000,
		  nlag = 10)
	plot(ESVF, xlim = c(0,10000))

}


