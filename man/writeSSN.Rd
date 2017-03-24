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
ssn <- importSSN(system.file("lsndata/MiddleFork04.ssn", package = "SSN"),
    predpts = "pred1km")

writeSSN(ssn, filename = paste(tempdir(),"/tempFile.ssn", sep = ""))

}
