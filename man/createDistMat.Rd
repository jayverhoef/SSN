\name{createDistMat}
\alias{createDistMat}

\title{
Calculate Hydrologic Distances for a SpatialStreamNetwork Object
}
\description{
Creates a collection of (non-symmetric) matrices containing pairwise downstream
hydrologic distances between sites in a SpatialStreamNetwork object
}
\usage{
createDistMat(ssn, predpts = NULL, o.write = FALSE, amongpreds = FALSE)
}

\arguments{
  \item{ssn}{
    a \link{SpatialStreamNetwork-class} object
  }
  \item{predpts}{
    a valid predpoints ID from the ssn
  }
  \item{o.write}{
    If TRUE, overwrite existing distance matrices. Defaults to \code{FALSE}.
  }
  \item{amongpreds}{
    If TRUE, compute the distances between the prediction sites i. Defaults to
    \code{FALSE}.
  }
}
\details{
  A distance matrix that contains the hydrologic distance between any two sites
  in SpatialStreamNetwork object is needed to fit a spatial statistical model
  using the tail-up and tail-down autocovariance functions described in Ver Hoef and
  Peterson (2010). These models are implemented in R via \command{glmssn}
  in the SSN package. The hydrologic distance information needed to model the
  covariance between flow-connected (i.e. water flows from one location to the
  other) and flow-unconnected (i.e. water does not flow from one location to the
  other, but they reside on the same network) locations differs. The total
  hydrologic distance is a directionless measure; it represents the hydrologic
  distance between two sites, ignoring flow direction. The hydrologic distance
  from each site to a common downstream stream junction is used when creating
  models for flow-unconnected pairs, which we term downstream hydrologic
  distance. In contrast, the total hydrologic distance is used for modeling
  flow-connected pairs, which we term total hydrologic distance.

  A downstream hydrologic distance matrix provides enough information to meet
  the data requirements for both the tail-up and tail-down models. When two
  locations are flow-connected, the downstream hydrologic distance from the
  upstream location to the downstream location is greater than zero, but it is
  zero in the other direction. When two locations are flow-unconnected the
  downstream hydrologic distance will be greater than zero in both directions. A
  site's downstream hydrologic distance to itself is equal to zero. The format
  of the downstream hydrologic distance matrix is efficient because distance
  information needed to fit both the tail-up and tail-down models is only stored
  once. As an example, a matrix containing the total hydrologic distance between
  sites is easily calculated by adding the downstream distance matrix to its
  transpose.

  The downstream hydrologic distances are calculated based on the binaryIDs and
  stored as matrices. The matrices are stored in a directory named \sQuote{distance},
  which is created by the createDistMat function within the .ssn directory. The
  distance directory will always contain at least one directory named \sQuote{obs},
  which contains a number of .RData files, one for each network that has observed
  sites residing on it. The naming convention for the files is based on the netID
  number (e.g. dist.net1.RData). Each matrix in the \sQuote{obs} folder contains
  the information to form a square matrix, which contains the downstream hydrologic
  distance between each pair of observed sites on the network. Direction is
  preserved, with columns representing the FROM site and rows representing the
  TO site. Row and column names correspond to the pid attribute for each site.

  If the argument \code{predpts} is specified in the call to the function, the
  downstream hydrologic distances between the observed and prediction sites will
  also be computed. A new directory is created within the distance directory, with
  the name corresponding to the predpoints ID (e.g. \dQuote{preds}). A sequence
  of .RData files is created within this directory, similar to the structure for
  the observed sites, except that two objects are stored for each network that
  contains \emph{both} observed and prediction sites. The letters \code{a} and
  \code{b} are used in the naming convention to distinguish between the two objects
  (e.g. dist.net1.a and dist.net1.b). The matrices that these objects represent
  are not necessarily square. In matrices of type \code{a}, rows correspond to
  observed locations and columns to prediction locations. In contrast, rows
  correspond to prediction locations and columns to observed locations in matrices
  of type \code{b}. Direction is also preserved, with columns representing the
  FROM site and rows representing the TO site in both object types. Again, row
  and column names correspond to the pid attribute for each site.

  If the argument \code{amongpreds} is set to TRUE, the downstream hydrologic
  distances will also be computed between prediction sites, for each network.
  Again these are stored within the distance directory with the name
  corresponding to the predpoints ID. The naming convention for these prediction
  to prediction site distance matrices is the same as the distance matrices stored
  in the \sQuote{obs} directory (e.g. dist.net1.RData). These extra distance matrices
  are needed to perform block Kriging using the \command{glmssn}
}
\value{
  The \command{createDistMat} function creates a collection of hierarchical
  directories in the \code{ssn@path} directory, which store the pairwise
  distances between sites associated with the \link{SpatialStreamNetwork-class}
  object. See details section for additional information.
}
\references{
  \cite{Ver Hoef, J.M. and Peterson, E.E. (2010) A moving average approach to
      spatial statistical models of stream networks. The Journal of the American
      Statistical Association, \bold{105(489)}, 22--24}
}
\author{
  Erin E. Peterson & Rohan Shah \email{support@SpatialStreamNetworks.com}
}
\seealso{
  \code{\link{SpatialStreamNetwork-class}}, \code{\link{importSSN}}, \code{\link{createSSN}}, \code{\link{glmssn}}
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

# create distance matrix among observed data points
createDistMat(mf04p, o.write = TRUE)

# create distance matrix among observed data points
#     and between observed and prediction points
createDistMat(mf04p, predpts = "pred1km", o.write = TRUE)

# NOT RUN include prediction to prediction site distances
# createDistMat(mf04p, predpts = "pred1km", o.write = TRUE, amongpreds = TRUE)

}
