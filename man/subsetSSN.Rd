\name{subsetSSN}
\alias{subsetSSN}
\title{
Subset a SpatialStreamNetwork Object
}
\description{
  The \command{subsetSSN} function returns an SpatialStreamnetwork object that
  has been subset based on a logical expression.
}
\usage{
subsetSSN(ssn, filename = "", subset, clip = FALSE)
}
\arguments{
  \item{ssn}{
   a \link{SpatialStreamNetwork-class} object
}
  \item{filename}{
   the file path to the new .ssn folder, in string format. When writing, include
   the .ssn folder in the path
}
  \item{subset}{
   logical expression indicating which elements or rows to keep: missing values
   are taken as false
}
  \item{clip}{
   default = FALSE. If TRUE, create a subset of the edges and prediction sites,
   based on the same logical expression used to subset the observed sites, and
   export the subset data to the new .ssn folder
}
}

\value{
 \code{subsetSSN} returns an object of class \code{\link[=SpatialStreamNetwork-class]{SpatialStreamNetwork}}.
  It also creates and stores an SQLite database, binaryID.db, within the new .ssn
  directory.
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

ssn.sub1 <- subsetSSN(ssn, filename = 
  paste(tempdir(),"/subset1.ssn", sep = ""), subset = Summer_mn > 13)
ssn.sub2 <- subsetSSN(ssn, filename = 
  paste(tempdir(),"/subset2.ssn", sep = ""), subset = netID == 1, clip = TRUE)


}
