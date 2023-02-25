\name{splitPredictions}
\alias{splitPredictions}

\title{
  Split Prediction Sets in a SpatialStreamNetwork Object
}
\description{
  The \command{splitPrediction} function is used to split prediction sets in a
  \link[=SpatialStreamNetwork-class]{SpatialStreamNetwork} object into smaller
  prediction sets. It returns a SpatialStreamNetwork object with additional
  prediction sets based on equal interval splits, a factor value stored
  within the prediction set, or a logical expression.
}
\usage{
  splitPredictions(ssn, predpointsID, chunksof, by, subset, new.id)
}

\arguments{
  \item{ssn}{
    a \link[=SpatialStreamNetwork-class]{SpatialStreamNetwork} object
}
  \item{predpointsID}{
    a character string representing the prediction points ID
}
  \item{chunksof}{
    numeric value representing the size of the new prediction sets. The
    existing prediction set is split equally to produce multiple
    prediction sets of this size
}
  \item{by}{
    character string representing the column name of type factor or
    integer that the split will be based on
}
\item{subset}{
    logical expression indicating which elements or rows to keep; missing values
    are taken as false
}
  \item{new.id}{
    character string representing the new prediction points ID. This
    value is only specified when the subset method is used
}
}
\details{
  Three methods have been provided to split prediction sets: \code{chunksof},
  \code{by}, and \code{subset}. The \code{chunksof} method is used to
  split the existing prediction set into multiple equally-sized
  prediction sets. The \code{by} method is used if the prediction set is
  to be split into multiple new prediction sets based on an existing
  column of type factor or integer. The \code{subset} method is used to
  create one new prediction setbased on a logical expression. When the
  \code{subset} method is used, the prediction points ID may be
  specified using the \code{new.id} argument. Note that, only one method
  may be specified when the \command{splitPredictions} function is called.


}
\value{
  The \command{splitPredictions} function returns an object of class
  \code{\link[=SpatialStreamNetwork-class]{SpatialStreamNetwork}} that
  contains one or more new prediction sets. Shapefiles of the new
  prediction sets are written to the .ssn directory designated in the
  ssn@path slot. Distances matrices for the predicted and observed locations are
  also calculated and stored in the .ssn directory.
}

\author{
Rohan Shah \email{support@SpatialStreamNetworks.com}
}

\seealso{
\link[=SpatialStreamNetwork-class]{SpatialStreamNetwork}, \link{subsetSSN}
}
\examples{
library(SSN)

# for examples, copy MiddleFork04.ssn directory to R's temporary directory
copyLSN2temp()

## Import the SpatialStreamNetwork object
mf04 <- importSSN(paste0(tempdir(),'/MiddleFork04.ssn'),
  predpts = "pred1km")

# Split using the chunksof method
split1 <- splitPredictions(mf04, "pred1km", chunksof = 100)
summary(split1)

## Split using the by method
split2 <- splitPredictions(mf04, "pred1km", by = "netID")
summary(split2)

## Split using the subset method
split3 <- splitPredictions(mf04, "pred1km", subset = netID == 1,
     new.id="netID-1")
summary(split3)

## Split the predictions after creating a glmssn object
#mf04.glmssn <- glmssn(Summer_mn ~ ELEV_DEM + SLOPE, mf04,
#    CorModels = c("Exponential.tailup", "Exponential.taildown",
#    "Exponential.Euclid"),addfunccol = "afvArea")
#mf04.glmssn$ssn.object <- splitPredictions(mf04.glmssn$ssn.object, "pred1km",
#    subset = netID == 1, new.id="netID-1")
#pred.split<- predict(mf04.glmssn, "netID-1")
#plot(pred.split)

}

