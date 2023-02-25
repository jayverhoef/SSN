\name{copyLSN2temp}
\alias{copyLSN2temp}
\title{
Copies the lsn directory MiddleFork04.ssn to R's temporary directory
}
\description{
  Copies the lsn directory MiddleFork04.ssn to R's temporary directory so the examples do not write to library or any other places
}
\usage{
  copyLSN2temp()
}

\details{
  Copies the lsn directory MiddleFork04.ssn to R's temporary directory
}
\value{
  MiddleFork04.ssn on R's temporary directory 
}

\author{
  Jay Ver Hoef \email{support@SpatialStreamNetworks.com}
}

\examples{

library(SSN)
copyLSN2temp()
#getwd()
#setwd(tempdir())
#getwd()
#if unix-alike use: system('ls')
#if windows use: system('dir')

}
