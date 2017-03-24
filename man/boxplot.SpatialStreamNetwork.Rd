\name{boxplot.SpatialStreamNetwork}
\alias{boxplot.SpatialStreamNetwork}
\title{
Box-and-whisker plots for data within SpatialStreamNetwork objects.
}
\description{
The \command{boxplot} function creates box-and-whisker plots for data within
\link{SpatialStreamNetwork-class}objects.
}
\usage{
\method{boxplot}{SpatialStreamNetwork}(x, variable, ...)
}
\arguments{
  \item{x}{
  an object of class \link{SpatialStreamNetwork-class}
  }
  \item{variable}{
  the variable (in quotes) for which the boxplots are being created,
  or a formula, such as \code{y ~ grp}, where \code{y} is a numeric vector of 
  data values and \code{grp} is a grouping of variables (usually a factor).
  }
  \item{...}{
    see \code{\link{boxplot}} for additional arguments
  }
}
\details{
  \command{boxplot} is a generic function that has been adapted for 
  \link{SpatialStreamNetwork-class}objects. Use \command{names} to get a list 
  of the variable names within the \code{SpatialStreamNetwork} object; the boxplot 
  boxplot will only work for the observed data set.
}
\value{
  A graph is produced, and a list with the following components:
  \item{stats}{
    a matrix, each column contains the extreme of the lower whisker,
    the lower hinge, the median, the upper hinge and the extreme of
    the upper whisker for each group/plot. If all the inputs have the
    same class attribute, then so will this component.
  }
  \item{n}{
    a vector with the number of observations in each group.
  }
  \item{conf}{
    a matrix where each column contains the lower and upper
    extremes of the notch.
  }
  \item{out}{
    the values of any data points which lie beyond the extremes
    of the whiskers.
  }
  \item{group}{
    a vector of the same length as out whose elements indicate
    to which group the outlier belongs.
  }
  \item{names}{
    a vector of names for the groups
    }
}
\references{
see \code{\link{boxplot}}
}
\author{
Jay Ver Hoef \email{support@SpatialStreamNetworks.com}
}
\seealso{
  \code{\link{boxplot.stats}}
}
\examples{

library(SSN)
# NOT RUN 
# mf04 <- importSSN(system.file("lsndata/MiddleFork04.ssn", 
#	        package = "SSN"), o.write = TRUE)
# use SpatialStreamNetwork object mf04 that was already created
data(mf04)

boxplot(mf04, "Summer_mn")
boxplot(mf04, Summer_mn ~ STREAMNAME, main = "Summer_mn ~ STREAMNAME", 
      col = "gray", xlab = "STREAMNAME", ylab = "Summer Mean Temperature")

}
