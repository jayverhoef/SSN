\name{getStreamDistMat}
\alias{getStreamDistMat}
\title{Extract the stream network distance matrix from SSN objects}
\description{
  The \command{getStreamDistMat} function extracts the stream network distance matrix for either observation data or prediction data, from the specified \code{SpatialStreamNetwork} object.
}
\usage{
getStreamDistMat(x, Name = "obs")
}
\arguments{
  \item{x}{
    an object of class \link{SpatialStreamNetwork-class}.  Note that the \link{createDistMat} function needs to be run on an imported \code{SpatialStreamNetwork} object first in order to create the distance matrix.
  }
  \item{Name}{
    the internal name of the data set in the object \code{x}. For observed values, this will always be "Obs", the default.  To get a stream network distance matrix for a prediction data set, the name of the data set must be given, in quotes.
  }
}
\details{
The internal \code{Name} for observed data in objects of class \code{SpatialStreamNetwork} is "Obs" and it is the default. If another \code{Name} is specified, it must represent a prediction data set in the \code{SpatialStreamNetwork-class}. For \code{SpatialStreamNetwork} objects, these names are obtained using the call ssn@predpoints@ID.  

Note that these are not traditional distance matrices because they are asymmetric. The matrices contain the distance from one point to the common junction of both points, so they are asymmetric.  For example, if two points are flow-connected, the distance from the point lower in the network to the one higher in the network is 0, while the distance from the higher point to the lower point is  > 0. The convention is that the "from" point, to the common junction, is along the top of the matrix (with the column labels), and the "to" point, to the common junction, is along the left side of the matrix (with the row labels).  From this matrix, it is possible to get total stream distance between any two points, an indicator matrix of flow-connectedness, etc. See examples for additional details.
}

\value{
  A \code{\link{list}} of matrices.  Note that distances are only computed within networks. For "Obs" data, a matrix of distances is returned for each network, labeled "dist.net1", "dist.net2", etc., for the first and second network, etc.  For prediction matrices, there are "from" and "to" matrices for both observed sites and predictions sites.  The convention is that "from" are again the columns, and "to" are again the rows, but the label "a" is for from prediction sites to observation sites, and the label "b" is for from observation sites to predictions sites.  Thus, the list of prediction matrices are labeled "dist.net1.a" for distance to common junction from prediction sites along the columns, to observation sites along the rows, for the first network.  A prediction matrix labeled "dist.net1.b" contains distances to the common junction from observation sites along the columns to prediction sites along the rows, for the first network. If the argument \code{amongPreds = TRUE} was used for the function \code{createDistMat}, then the distance to common junction among prediction sites is returned, using the same labelling convention as for among observation sites.  That is, the matrices for each network will be labeled "dist.net1", "dist.net2", etc., for the first and second network, etc. 
}

\author{
Jay Ver Hoef \email{support@SpatialStreamNetworks.com}
}


\examples{
library(SSN)
# NOT RUN
#mf04 <- importSSN(system.file("lsndata/MiddleFork04.ssn",
#  package = "SSN"), o.write = TRUE)
#  use SpatialStreamNetwork object mf04p that was already created
data(mf04p)
# for examples, copy MiddleFork04.ssn directory to R's temporary directory
copyLSN2temp()
#make sure mf04p has the correct path, will vary for each users installation
mf04p <- updatePath(mf04p, paste0(tempdir(),'/MiddleFork04.ssn'))

names(mf04p)

distObs <- getStreamDistMat(mf04p)
str(distObs)
distObs$dist.net1[1:5,1:5]

# get total in-stream distance between all pairs of points
strDistNet2 <- distObs$dist.net2 + t(distObs$dist.net2)
strDistNet2[5:10,5:10]

# maximum distance to common junction between two sites
a.mat <- pmax(distObs$dist.net2,t(distObs$dist.net2))
a.mat[5:10,5:10]

# minimum distance to common junction between two sites
# sites with 0 minimum distance are flow-connected
b.mat <- pmin(distObs$dist.net2,t(distObs$dist.net2))
b.mat[5:10,5:10]

# get distance matrices between observed sites and prediction sites
distPred1km <- getStreamDistMat(mf04p, Name = "pred1km")
str(distPred1km)
distPred1km$dist.net1.a[1:5,1:5]

# create distance matrix among prediction sites
# note these sites only occur on the second network
# this is useful for block prediction
createDistMat(mf04p, predpts = "CapeHorn", o.write = TRUE, amongpreds = TRUE)
distCape <- getStreamDistMat(mf04p, Name = "CapeHorn")
str(distCape)
distCape$dist.net2[1:5,1:5]
}

