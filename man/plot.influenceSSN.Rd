\name{plot.influenceSSN}
\alias{plot.influenceSSN}

\title{
Plotting Method for influenceSSN Objects
}
\description{
plot.influenceSSN is a generic plot function that has been adapted for
\link{influenceSSN-class} objects that have been created from the residuals function.
}
\usage{
\method{plot}{influenceSSN}(x, color.palette = NULL, nclasses = NULL, inflcol = "_resid_",
breaktype = "quantile", brks=NULL, pch = 19, ...)
}

\arguments{
  \item{x}{
   an object of class influenceSSN.
}
  \item{color.palette}{
   a color palette for plotting points. The default is rainbow(nclasses,
   start = .66, end = .99). The number of colors should equal the number of
   classes. See \code{\link{palette}} for many ways to create palettes.
}
  \item{nclasses}{
   the number of classes for coloring the predictions (or standard errors) according
   to their value.  The default is 10.
}
  \item{inflcol}{
   an influence diagnostic or cross-validation variable name in the
   \code{\link[=influenceSSN-class]{influenceSSN}} object. If NULL (default),
   just locations are plotted.  If a variable is specified, it will be colored
   according to its value.
}
  \item{breaktype}{
   The method for breaking the response values into classes for
   coloring while plotting.  A character argument that must be one of "quantile"
   (default), "even", or "user".
}
  \item{brks}{
    if breaktype = "user", the break values must be specified here as a
    vector or matrix using c(...) or cbind(...). The sorted unique
    values are used as break points (together with the min and max of the variable
    being plotted if required)
}
  \item{pch}{
   either an integer specifying a symbol or a single character to be used as the
   default in plotting points. See \code{link{points}} for possible values and their
   interpretation. Note that only integers and single-character strings can be
   set as a graphics parameter (and not NA nor NULL).
}
  \item{\dots}{
   arguments to be passed to methods, such as graphical parameters (see \code{\link{par}}).
}
}
\details{
The \command{plot.influenceSSN} function creates a map showing data locations that
can be color-coded according to the values of the diagnostic or influence
variables.
}
\value{
Maps of stream networks, with the spatial distribution of the influence or
cross-validation variables shown.
}

\author{
Jay Ver Hoef \email{support@SpatialStreamNetworks.com}
}

\seealso{
  \code{\link{influenceSSN-class}}, \code{\link{residuals}}, \code{\link{plot.SpatialStreamNetwork}}
}
\examples{

library(SSN)
data(modelFits)
#make sure fitSp has the correct path, will vary for each users installation
fitSp$ssn.object@path <- system.file("lsndata/MiddleFork04.ssn", package = "SSN")

resids <- residuals(fitSp,cross.validation = TRUE)
plot(resids)

## plot using user-defined breakpoints
brks <- seq(-3,2,by=1)
plot(resids, nclasses = 6, inflcol = "_resid_",
    breaktype = "user", brks = brks, pch = 3)

## plot crossvalidation residuals
plot(resids, nclasses = 6, inflcol = "_resid.crossv_")

}

