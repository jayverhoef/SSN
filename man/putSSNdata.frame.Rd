\name{putSSNdata.frame}
\alias{putSSNdata.frame}
\title{putSSNdata.frame}
\description{
Replacet the data.frame in an Object of Class SpatialStreamNetwork
}
\usage{
putSSNdata.frame(DataFrame, x, Name = "Obs")
}

\arguments{
  \item{DataFrame}{
   data.frame to be placed into the \link{SpatialStreamNetwork-class} object
  }
  \item{x}{
   an object of class \code{SpatialStreamNetwork}, \code{influenceSSN}, \code{glmssn}, 
   or \code{glmssn.predict} 
  }
  \item{Name}{
   the internal name of the data set in the object \code{x}. For observed values,  
   this will always be "Obs", the default. 
}
}
\details{
The internal \code{Name} for observed data in objects of class \code{SpatialStreamNetwork}
is "Obs" and it is the default. If another \code{Name} is specified, it must 
represent a prediction data set in the \code{SpatialStreamNetwork-class}, 
\code{influenceSSN-class}, \code{glmssn-class}, or "glmssn.predict" object. For 
\code{SpatialStreamNetwork} objects, these names are obtained using the call 
ssn@predpoints@ID. For all other object classes, the names are
obtained using the call object$ssn.object@predpoints@ID. See examples for additional
details. 

Note that, the \code{DataFrame} must be of the same dimensions as the original
data.frame in the object \code{x}.  
}
\value{
  Returns an object of the same class as \code{x}.
}
\author{
Jay Ver Hoef \email{support@SpatialStreamNetworks.com}
}
\seealso{
\code{\link{getSSNdata.frame}}, \code{\link{SpatialStreamNetwork-class}}, \code{\link{influenceSSN-class}},
\code{\link{glmssn-class}}
}

\examples{

library(SSN)
#for examples, copy MiddleFork04.ssn directory to R's temporary directory
copyLSN2temp()
# NOT RUN
# Create a SpatialStreamNetork object that also contains prediction sites
#mf04 <- importSSN(paste0(tempdir(),'/MiddleFork04.ssn', o.write = TRUE))
#use mf04 SpatialStreamNetwork object, already created
data(mf04)
#for examples only, make sure mf04p has the correct path
#if you use importSSN(), path will be correct
mf04 <- updatePath(mf04, paste0(tempdir(),'/MiddleFork04.ssn'))

# Take out the data.frame, make a change and put it back
obs.df <- getSSNdata.frame(mf04)
obs.df$Year_cat <- as.factor(obs.df$SampleYear)
mf04 <- putSSNdata.frame(obs.df, mf04)

}
