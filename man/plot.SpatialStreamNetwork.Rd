\name{plot.SpatialStreamNetwork}
\alias{plot.SpatialStreamNetwork}
\alias{summary.SpatialStreamNetwork}
\title{
Plotting Method for SpatialStreamNetwork Objects
}
\description{
  \command{plot.SpatialStreamNetwork} is a generic plot function that has been adapted for
  SpatialStreamNetwork objects that have been created in SSN.
}
\usage{
\method{plot}{SpatialStreamNetwork}(x, VariableName=NULL, color.palette= NULL, 
  nclasses = NULL, breaktype = "quantile", brks = NULL, PredPointsID = NULL,
  add = FALSE, addWithLegend=FALSE, lwdLineCol = NULL, lwdLineEx = 1, 
  lineCol = "black", ...)
}
\arguments{
  \item{x}{
    an object of class \code{\link[=SpatialStreamNetwork-class]{SpatialStreamNetwork}}.
  }
  \item{VariableName}{
    a response variable name in the data.frame of observed data in the
    SpatialStreamNetwork object. If NULL (default), just locations are plotted.
    If a variable is specified, it will be colored according to its value.
  }
   \item{color.palette}{
    a color palette for plotting points. The default is rainbow(nclasses, start = .66, end = .99).
    The number of colors should equal the number of classes. See \code{\link{palette}} for many
    ways to create palettes.
  }
   \item{nclasses}{
    the number of classes for coloring the predictions (or standard errors) according
    to their value.  The default is 10. If brks = c(...) is specified, then nclasses
    is automatically set to the number of unique breaks + 1.
  }
  \item{breaktype}{
    the method for breaking the response values into classes for coloring while
    plotting.  A character argument that must be one of "quantile" (default),
    "even", or "user".
  }
  \item{brks}{
    if breaktype = "user", the break values must be specified here as a
    vector or matrix using c(...) or cbind(...). The sorted unique
    values are used as break points (together with the min and max of the variable being
    plotted if required).
  }
  \item{PredPointsID}{
    a string representing the internal name of the prediction sites data set, which
    will be added to the plot. Default is NULL.
  }
  \item{add}{
    logical indicating whether the predictions should be added to an existing plot,
    such as a plot of the stream network and observed locations. Use this if there
    is no legend. Default is FALSE.
  }
  \item{addWithLegend}{
    logical indicating whether the predictions should be added to an existing plot,
    such as a plot of colored values for observed data. Use this when there is a legend.
    Default is FALSE.
  }
  \item{lwdLineCol}{
    a column name in the lines data frame to be used for line width expansion. This
	will most likely be the name of the additive function column, but others could be
	used.
  }
  \item{lwdLineEx}{
    an expansion multiplier to create line widths for the values contained in lwdLineCol.
  }
  \item{lineCol}{
    a color for the lines forming the stream network.  Default is "black".
  }
  \item{\dots}{
    arguments to be passed to methods, such as graphical parameters (see \code{\link{par}}).
  }
}
\details{
The \command{plot.SpatialStreamNetwork} function creates a map showing data locations that
can be color-coded according to the values of observed variables. Prediction
locations can also be added to existing plots of observed values.
}
\value{
Maps of stream networks
}
\author{
Jay Ver Hoef \email{support@SpatialStreamNetworks.com}
}

\seealso{
  \code{\link{SpatialStreamNetwork-class}}, \code{\link{plot}}
}
\examples{

library(SSN)
# Create a SpatialStreamNetork object that also contains prediction sites
#undebug(importSSN)
mf04p <- importSSN(system.file("lsndata/MiddleFork04.ssn", 
	package = "SSN"), predpts = "pred1km", o.write = TRUE)
names(mf04p)
summary(mf04p)

#generic plotting includes stream network and observed locations
plot(mf04p)

#plot including the color coding the response variable
plot(mf04p, "Summer_mn")

#plot using user-defined breakpoints
plot(mf04p, "Summer_mn", breaktype = "user", brks = seq(8,16,by=1))

#pass normal plotting arguments, such as xlab and ylab, to plot
plot(mf04p, "Summer_mn", xlab = "x-coordinate", ylab = "y-coordinate")

# plot observations and prediction values
plot(mf04p, "Summer_mn", cex = 2, xlab = "x", ylab = "y")
plot(mf04p, PredPointsID = "pred1km", addWithLegend = TRUE)

}
