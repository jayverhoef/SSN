\name{updatePath}
\alias{updatePath}


\title{
  Update Path Slot in SpatialStreamNetwork Object }

\description{
Updates the path slot in an existing SpatialStreamNetwork object based on a
user-defined filepath.  }

\usage{ updatePath(ssn, filepath) }

\arguments{
  \item{ssn}{
    a \code{\link[=SpatialStreamNetwork-class]{SpatialStreamNetwork}} object
}
  \item{filepath}{
    path name to the .ssn folder, in string format including quotes. Also
    include the .ssn folder in the path name
}
}
\details{
At times, it may be necessary to move a .ssn directory, which is linked
to a \code{\link[=SpatialStreamNetwork-class]{SpatialStreamNetwork}}
object in an R workspace. If the .ssn directory is moved, the path slot
must be updated before using the \code{glmssn} function. The
\code{updatePath} function serves this purpose.
}


\author{
Erin E. Peterson \email{support@SpatialStreamNetworks.com}
}

\examples{
library(SSN)

## UPDATE PATH FOR SpatialStreamNetwork object
## NOT RUN
# mf04p <- importSSN(system.file("lsndata/MiddleFork04.ssn",
#	  package = "SSN1.1"), predpts = "pred1km", o.write = TRUE)
#   use SpatialStreamNetwork object mf04p that was already created
data(mf04p)

## Update the path, which will vary for each users installation
mf04p <- updatePath(mf04p, system.file("lsndata/MiddleFork04.ssn", package = "SSN"))


## UPDATE PATH FOR glmssn object
## get some model fits stored as data objects
data(modelFits)
## NOT RUN
## Fit a model to binary data
## binSp <- glmssn(MaxOver20 ~ ELEV_DEM + SLOPE, mf04p,
##   CorModels = c("Mariah.tailup", "Spherical.taildown"),
##   family = "binomial", addfunccol = "afvArea")

## make sure binSp has the correct path, will vary for
## each users installation
binSp$ssn.object <- updatePath(binSp$ssn.object, 
	system.file("lsndata/MiddleFork04.ssn", package = "SSN"))

}
