\name{importSSN}
\alias{importSSN}
\title{
Import an object of class SpatialStreamNetwork
}
\description{
The \command{importSSN} function reads spatial data from a .ssn folder and to  
create a \link[=SpatialStreamNetwork-class]{SpatialStreamNetwork} object
}
\usage{
importSSN(filepath, predpts = NULL, o.write = FALSE)
}
\arguments{
  \item{filepath}{
    path name to the .ssn folder, in string format including quotes. Also 
    include the .ssn folder in the path name
  }
  \item{predpts}{
    default = NULL. Prediction points shapefile name enclosed in quotes. When
    writing, omit the .shp extension. Prediction points must reside in the .ssn 
    directory and be generated from the same landscape network as the other 
    spatial data in the .ssn directory
  }
  \item{o.write}{
    default = FALSE. If TRUE, overwrite existing binaryID.db files
}
}
\details{
  The importSSN function imports spatial data from a .ssn folder to create a
  \link{SpatialStreamNetwork-class} object. The information contained in the .ssn
  folder is generated in a geographic information system using the Spatial Tools
  for the Analysis of River Systems (STARS) tools for ArcGIS version 9.3.1. A
  detailed description of the spatial data format is provided in Peterson (2011).

  The information contained in the netID text files is imported into an SQLite
  database, binaryID.db, which is stored in the .ssn directory. This
  information is used internally by \code{createDistMat} and \code{glmssn} to
  calculate the data necessary to fit a spatial statistical model to stream
  network data. If o.write = TRUE (o.write = FALSE is the default) and a binaryID.db 
  file already exists within the .ssn directory, it will be overwriten when the 
  \code{SpatialStreamNetwork} object is created. 

  A \code{SpatialStreamNetwork} object may contain multiple sets of prediction points,
  which are contained in separate shapefiles. One prediction point shapefile may
  be imported using importSSN. The \code{importPredpts} function allows users to
  import additional sets of prediction sites to a an existing
  \code{SpatialStreamNetwork} object. The prediction points ID is set to the base
  name the shapefile.

}
\value{
  \code{importSSN} returns an object of class \code{\link[=SpatialStreamNetwork-class]{SpatialStreamNetwork}}.
  It also creates and stores a SQLite database, binaryID.db, within the .ssn
  directory.

}
\references{
  \cite{Peterson E.E.(2011)STARS: Spatial Tools for the Analysis of River Systems:
  A tutorial. CSIRO Technical Report EP111313. 42p.}
}
\author{
Erin E. Peterson \email{support@SpatialStreamNetworks.com}
}

\seealso{
   \code{\link{importPredpts}} for adding prediction points after a
    \link[=SpatialStreamNetwork-class]{SpatialStreamNetwork} object has been created. \code{\link{createDistMat}} to create distance matrices among points, both among observed, between observed and predicted, and among predicted. \code{\link{createSSN}} for creating SSN objects from scratch for simulation purposes.
}

\examples{

library(SSN)
# Create a SpatialStreamNetwork object that does not contain prediction points
mf04 <- importSSN(system.file("lsndata/MiddleFork04.ssn", 
	package = "SSN"), o.write = TRUE)

# Create a SpatialStreamNetork object that also contains prediction sites
mf04p <- importSSN(system.file("lsndata/MiddleFork04.ssn", 
	package = "SSN"), predpts = "pred1km", o.write = TRUE)
names(mf04p)

#NOT RUN Some graphics to explore imported object
#hist(mf04, "Summer_mn")
#boxplot(mf04, "Summer_mn")
#plot(mf04, cex = .8, xlab = "x", ylab = "y")
#plot(mf04p, PredPointsID = "pred1km", add = TRUE, pch = 1) #add to previous plot

}

