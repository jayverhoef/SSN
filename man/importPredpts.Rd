\name{importPredpts}
\alias{importPredpts}

\title{
  Import Prediction Points into a SpatialStreamNetwork or glmssn Object
}
\description{
  Prediction points residing in a .ssn directory are imported into an existing
  object of class \link{SpatialStreamNetwork-class} or \link{glmssn-class}
}
\usage{
importPredpts(target, predpts, obj.type)
}

\arguments{
  \item{target}{
    a \code{SpatialStreamNetwork-class} or \code{glmssn-class} object
}
  \item{predpts}{
    Prediction points shapefile name, enclosed in quotes. When
    writing, omit the .shp extension. Prediction points must reside in the 
    .ssn directory and be generated from the same landscape network as the other 
    spatial data in the \code{SpatialStreamNetwork} object
}
  \item{obj.type}{
    the class of the target. For a \code{SpatialStreamNetwork} object,
    \code{target = "ssn"}. For a \code{glmssn-class}
    object, \code{target = "glm"}
}
}
\details{
  importPredpts imports a shapefile of prediction points residing in the .ssn
  directory into an existing \code{SpatialStreamnetwork} or \code{glmssn-class}
  object. The spatial datasets residing the .ssn folder are generated in a 
  geographic information system using the Spatial Tools for the Analysis of River 
  Systems (STARS) tools for ArcGIS version 9.3.1. A detailed description of the 
  spatial data format is provided in Peterson (2011).

}
\value{
  importPredpts returns an object of class "SpatialStreamNetwork" or "glmssn".
  An additional predpoints slot is populated in the object
}
\references{
 \cite{Peterson E.E.(2011)STARS: Spatial Tools for the Analysis of River Systems:
  A tutorial. CSIRO Technical Report EP111313. 42p.}
}
\author{
  Erin E. Peterson \email{support@SpatialStreamNetworks.com}
}
\seealso{
  \code{\link{importSSN}}, \code{\link{SpatialStreamNetwork-class}}, and
  \code{\link{glmssn-class}}
}
\examples{

library(SSN)
#mf04 <- importSSN(system.file("lsndata/MiddleFork04.ssn", 
#  package = "SSN"), o.write = TRUE)
#  use SpatialStreamNetwork object mf04 that was already created
data(mf04)
# for examples, copy MiddleFork04.ssn directory to R's temporary directory
copyLSN2temp()
#make sure mf04p has the correct path, will vary for each users installation
mf04 <- updatePath(mf04, paste0(tempdir(),'/MiddleFork04.ssn'))

mf04p <- mf04
# add existing prediction points on 1 km spacing
mf04p <- importPredpts(target = mf04p, predpts = "pred1km", obj.type = "ssn")
# get names and verify that pred1km has been added
names(mf04p)    
 
# add dense set of prediction points from Knapp stream
mf04p <- importPredpts(target = mf04p, predpts = "Knapp", obj.type = "ssn")
# get names and verify that Knapp has been added
names(mf04p)    

# add dense set of prediction points from CapeHorn stream
mf04p <- importPredpts(target = mf04p, predpts = "CapeHorn", obj.type = "ssn")
# get names and verify that CapeHorn has been added
names(mf04p)   
 
# create distance matrices, needed for prediction with stream network models
# NOT RUN
#createDistMat(mf04p, "pred1km", o.write = TRUE)
# for block prediction, we need distance among prediction points
#createDistMat(mf04p, "Knapp", o.write = TRUE, amongpreds = TRUE)
#createDistMat(mf04p, "CapeHorn", o.write = TRUE)

# Add prediction points to a glmssn object
# use models that have been created already
data(modelFits)
#make sure mf04 has the correct path, will vary for each users installation
fitSp$ssn@path <- system.file("lsndata/MiddleFork04.ssn", package = "SSN")

#use model named fitSp; NOT RUN; already imported
#fitSp <- importPredpts(target = fitSp, predpts = "pred1km", 
#  obj.type = "glm")
# now we can make predictions; make sure distance matrix for "pred1km" has
# been created
# NOT RUN
#fitSpPred <- predict(fitSp,"pred1km")
#plot(fitSpPred)
#fitSp <- importPredpts(target = fitSp, predpts = "Knapp", 
#  obj.type = "glm")
# NOT RUN
#fitSpPredKnapp <- predict(fitSp,"Knapp")
#plot(fitSpPredKnapp)

}
