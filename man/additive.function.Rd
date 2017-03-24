\name{additive.function}
\alias{additive.function}

\title{
Generate an Additive Function Value
}
\description{
Generate an additive function value based on a proportional influence variable
into an additive function value
}
\usage{
additive.function(ssn, VarName, afvName)
}

\arguments{
  \item{ssn}{
	a \link{SpatialStreamNetwork-class} object
}
  \item{VarName}{
	The name of the the variable that will be used to calculate the additive
	function value. The data.frame \code{ssn@data} must contain a column with
	this name.
}
  \item{afvName}{
	The name assigned to the column of additive function values, which are added
	to the ssn@data data.frame object, as well as the data.frames for the observed
	and prediction sites.
}
}
\details{
	Calculating the additive function values (AFVs) is a two step process; first
	the \code{VarName} values are used to calculate the segment proportional
	influences (PIs). Then the segment PI's are used to calculate the AFVs for
	each line segment, observed site, and prediction site in the \link{SpatialStreamNetwork-class}
	object. A detailed description of the segment PIs and the steps used to calculate
	AFVs are provided in Peterson and Ver Hoef (2010; Appendix A). The
	AFVs can also be calculated using the Spatial Tools for the Analysis of River
	Systems (STARS), which is a custom ArcGIS (version 9.3.1) toolbox.
}
\value{
The SpatialStreamNetwork object, \code{ssn}, with a new column named \code{VarName}
included in the data.frames for the lines, observed sites, and prediction sites
to hold the AFVs.
}
\references{
\cite{Peterson, E. E. and Ver Hoef, J. M. (2010) A mixed-model moving-average approach
to geostatistical modeling in stream networks. \emph{Ecology} \bold{91(3)},
644--651.}

\cite{Peterson E.E.(2011)STARS: Spatial Tools for the Analysis of River Systems:
A tutorial. CSIRO Technical Report EP111313. 42p.}
}
\author{
Rohan Shah \email{support@SpatialStreamNetworks.com}
}
\examples{

library(SSN)
#use mf04p SpatialStreamNetwork object, already created
data(mf04p)
# for examples, copy MiddleFork04.ssn directory to R's temporary directory
copyLSN2temp()
#make sure mf04p has the correct path, will vary for each users installation
mf04p <- updatePath(mf04p, paste0(tempdir(),'/MiddleFork04.ssn'))

#Calculate an additive function value based on an existing column.
names(mf04p@data)
mf04p <- additive.function(mf04p, "h2oAreaKm2", "areaAFV")
#notice that a column called afvArea was already included, and "areaAFV" replicates it
# in the lines data
head(mf04p@data)
# and in the observed points data
head(getSSNdata.frame(mf04p))
# and in the prediction points data
head(getSSNdata.frame(mf04p, "pred1km"))

}
