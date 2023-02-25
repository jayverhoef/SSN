\name{plot.Torgegram}
\alias{plot.Torgegram}
\title{
Plotting Method for Torgegram Objects
}
\description{
  \command{plot.Torgegram} is a generic plot function that has been adapted for
  Torgegram objects, which are created using the Torgegram function. A Torgegram
  object stores information used to construct an empirical semivariogram based
  on hydrologic distance. The \code{plot.Torgegram} function allwos the results
  to be presented separately for flow-connected and flow-unconnected sites.
}
\usage{
\method{plot}{Torgegram}(x, sp.relationship = c("fc", "fu"), 
  min.cex = 1.5, max.cex = 6, leg.auto = TRUE, main = "", ylab = "",	
  xlab = "Stream Distance", ... )
}
\arguments{
  \item{x}{
    an object of class Torgegram
  }
  \item{sp.relationship}{
    a string or character vector representing the in-stream spatial relationship
    to be plotted. "fc" specifies plotting of only flow-connected, and "fu" specifies
    plotting of only flow-unconnected. Default is both.
  }
   \item{min.cex}{
    Minimum character expansion size of the plotting symbols. Symbols are scaled
	according to how many pairs of points went into computing each bin of the
	semivariogram. The bin with the smallest sample size will be be plotted with
	this symbol size. The default is 1.5.
  }
  \item{max.cex}{
    Maximum character expansion size of the plotting symbols.  Symbols are scaled
    according to how many pairs of points went into computing each bin of the
    semivariogram. The bin with the largest sample size will be be plotted with
    this symbol size. The default is 6.
  }
  \item{leg.auto}{
    Logical, default to TRUE. Include a legend.
  }
  \item{main}{
    Title for plot
  }
  \item{ylab}{
    Label for y-axis
  }
  \item{xlab}{
    Label for x-axis
  }
  \item{...}{
	Other plotting arguments
  }
}
\details{
The \command{Torgegram} function creates a list of distances
and empirical semivariogram values, along with number of pairs of points in each
bin, for both flow-connected and flow-unconnected sites. Flow-connected locations
lie on the same stream network (share a common downstream junction) and water flows
from one location to the other. Flow-unconnected locations also lie on the same
stream network, but do not share flow. The output is of class Torgegram.
This is the default plotting method for this class.
}
\value{
Plot of empirical semivariogram values
}
\author{
Jay Ver Hoef \email{support@SpatialStreamNetworks.com}
}

\seealso{
  \code{\link{SpatialStreamNetwork-class}}, \code{\link{plot}}
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
