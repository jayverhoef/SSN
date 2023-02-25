\name{writeSSN}
\alias{writeSSN}
\title{
Write a SpatialStreamNetwork Object
}
\description{
  The \command{writeSSN} function writes a SpatialStreamnetwork object
  to a new .ssn directory.
}
\usage{
writeSSN(ssn, filename = "", o.write = FALSE)
}
\arguments{
  \item{ssn}{
   a \link{SpatialStreamNetwork-class} object
}
  \item{filename}{
   the file path to the new .ssn folder, in string format. When writing, include
   the .ssn folder in the path
}
  \item{o.write}{
    If TRUE, overwrite existing ssn. Defaults to \code{FALSE}.
} 
}

\value{
 \code{writeSSN} does not return an object. Instead, it creates a new .ssn
  directory that contains all of the information in the specified
  \code{SpatialStreamNetwork} object.
}

\author{
Erin E. Peterson \email{support@SpatialStreamNetworks.com}
}

\seealso{
  \link{SpatialStreamNetwork-class}
}
\examples{

library(SSN)
#for examples, copy MiddleFork04.ssn directory to R's temporary directory
copyLSN2temp()
# NOT RUN
# Create a SpatialStreamNetork object that also contains prediction sites
mf04p <- importSSN(paste0(tempdir(),'/MiddleFork04.ssn'), 
  predpts = "pred1km", o.write = TRUE)

writeSSN(mf04p, filename = paste0(tempdir(),"/tempFile.ssn"))

}
