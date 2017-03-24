
\name{plot.glmssn.predict}
\alias{plot.glmssn.predict}
\title{
Plot glmssn.predict Objects
}
\description{
  \command{plot.glmssn.predict} is a generic plot function that has been adapted
  for objects of class \code{glmssn.predict}.
}
\usage{
\method{plot}{glmssn.predict}(x, VariableName = NULL, VarPlot = "Both",
color.palette = rainbow(nclasses, start = 0.66, end = 0.99), nclasses =
10, breaktype = "quantile", dec.dig = 2, SEcex.min = 0.5, SEcex.max = 2,
brks = NULL, add = FALSE, ...)
}
\arguments{
  \item{x}{
    an object of class \code{glmssn.predict}
  }
  \item{VariableName}{
   name of variable to be plotted
  }
  \item{VarPlot}{
    a character argument that must be one of "Both", "Predictions", or "Standard Errors".
    Default is "Both", which colors predictions by their values and makes their size
    inversely proportional to the prediction standard errors.
  }
  \item{breaktype}{
    the method for breaking the predictions (or standard errors) into classes for
    coloring while plotting. A character argument that must be one of "quantile"
    (default), "even", or "user".
  }
  \item{brks}{
    if breaktype = "user", the break values must be specified here as a
    vector or matrix using c(...) or cbind(...). The sorted unique
    values are used as break points (together with the min and max of the variable
    being plotted if required)
  }
  \item{nclasses}{
    the number of classes for coloring the predictions (or standard errors) according
    to their value.  The default is 10. If brks = c(...) is specified, then nclasses
    is automatically set to the number of breaks + 1.
  }
  \item{color.palette}{
    a color palette for plotting points. The default is rainbow(nclasses, start = .66, end = .99).
    The number of colors should equal to the number of classes. See \code{\link{palette}}
    for many ways to create palettes.
  }
  \item{SEcex.min}{
    if VarPlot = "both", the minimum cex value when making point sizes is
    inversely proportional to the prediction standard errors. See \code{\link{par}}
    for more on cex.  Also see details below. Default is 1.
  }
  \item{SEcex.max}{
    if VarPlot = "both", the maximum cex value when making point sizes
    inversely proportional to the prediction standard errors. See \code{\link{par}}
    for more on cex.  Also see details below. Default is 3.
  }
  \item{dec.dig}{
    the number of decimal places to print in the legend.  Default is 2.
  }
  \item{add}{
    Logical value indicating whether the predictions should be added to an
    existing plot, such as a plot of colored values for observed data.
    Default is FALSE.
  }
  \item{\dots}{
    Arguments to be passed to methods, such as graphical parameters (see \code{\link{par}}).
  }
}
\details{
The \command{plot.glmssn.predict} function creates a map showing color-coded predictions or
prediction standard error values. When VarPlot = "Both", predictions values are colored
according to breaks.  The size of the points is inversely proportional to the
prediction standard errors. If SE is the standard error for a prediction, then the
size of the plotted point will be SEcex.max - (SEcex.max - SEcex.min)*(SE - min(SE))/(max(SE) - min(SE)),
where mins and maxs are over all SEs in the prediction set.  This is simply a
linear interpolator between SEcex.max and SEcex.min, specified by the user, with
larger points for smaller standard errors. So large points reflect the fact that
you have more confidence in those values and small points reflect the fact that
you have less confidence in the values. Two plot legends are included in this
case - one based on size and one on colour.

If the predictions are added to an existing plot, the printing of a second legend
is suppressed, but the minimum predicted value is added as text to the top of
the legend area, and the maximum predicted value is added as text to the bottom
of the legend area.  This option only makes sense if the breaks are matched to
those when plotting the observed values.  See the example below.
}
\value{
Maps of stream networks with prediction and prediction standard error values.
}

\author{
Jay Ver Hoef \email{support@SpatialStreamNetworks.com}
}

\seealso{
  \code{\link{predict}}
}
\examples{

library(SSN)
data(modelFits)
# model was fit for mf04p
data(mf04p)
# for examples, copy MiddleFork04.ssn directory to R's temporary directory
copyLSN2temp()
#make sure mf04p has the correct path, will vary for each users installation
mf04p <- updatePath(mf04p, paste0(tempdir(),'/MiddleFork04.ssn'))

#make sure fitSp has the correct path, will vary for each users installation
fitSp$ssn.object@path <- paste0(tempdir(),'/MiddleFork04.ssn')

# make sure the distance matrix is there
createDistMat(mf04p, predpts = "pred1km", o.write = TRUE)
## create predictions
ssnpred <- predict(fitSp, "pred1km")

##default graph
plot(ssnpred)

## max maximum size smaller
plot(ssnpred, SEcex.max = 1.5)

## predictions only
plot(ssnpred, VarPlot = "Predictions", breaktype = "quantile")

## change line width
plot(ssnpred, VarPlot = "Predictions", breaktype = "quantile", lwd = 2)

## change line type
plot(ssnpred, VarPlot = "Predictions", breaktype = "quantile", lwd = 2, lty = 2)

## standard errors only
plot(ssnpred, VarPlot = "Standard Errors", breaktype = "quantile")

## use even spacing for breaks
plot(ssnpred, VarPlot = "Standard Errors", breaktype = "even")

## use custom breaks and colors - specify four break points and four
## colours, but the actual range of standard errors requires a fifth break
## point and a fifth colour (see legend on RHS) chosen by default
plot(ssnpred, VarPlot = "Standard Errors", breaktype = "user",
 brks = seq(0,2.4,by=0.6), color.palette = c("darkblue", "purple",
   "green", "red"))

## add predictions to colored observations
brks <- as.matrix(plot(fitSp$ssn.object, "Summer_mn", cex = 2))
plot(ssnpred, add = TRUE, breaktype = "user", brks = brks, nclasses=length(brks),
    SEcex.max = 1)

}

