\name{SSN-package}
\alias{SSN-package}
\alias{SSN}
\docType{package}
\title{
Spatial Modeling on Stream Networks
}
\description{
Creates spatial stream network representations in R and fits spatial models.
}
\details{
\tabular{ll}{
Package: \tab SSN\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2011-02-09\cr
License: \tab GPL-2 \cr
LazyLoad: \tab yes\cr
}


The SSN package provides tools to fit generalized linear models with spatial 
autocorrelation using normal likelihood methods (including REML) and quasi-likelihood 
for Poisson and Binomial families. The spatial formulation is described in Ver Hoef 
and Peterson (2010) and Peterson and Ver Hoef (2010). The spatial data must be formatted
in a geographic information system (GIS) prior to importing it into R. Two custom 
ArcGIS toolboxes (v 9.3.1) have been provided to format the data: the Functional Linkage 
of Water Basins and Streams(FLoWS; Theobald et al. 2006) and the Spatial Tools for
the Analysis of River Systems (STARS) tooset (Peterson 2011).  


}
\author{
Jay Ver Hoef and Erin Peterson \email{support@SpatialStreamNetworks.com}
}
\references{
\cite{Ver Hoef, J. M. and Peterson, E. E. (2010) A moving average approach for 
spatial statistical models of stream networks (with discussion). \emph{Journal of the American Statistical Association} 
\bold{105}, 6--18. DOI: 10.1198/jasa.2009.ap08248.  Rejoinder pgs. 22 - 24.}

\cite{Peterson, E. E. and Ver Hoef, J. M. (2010) A mixed-model moving-average 
approach to geostatistical modeling in stream networks. \emph{Ecology} 
\bold{91(3)}, 644--651.}

\cite{Peterson E.E.(2011)STARS: Spatial Tools for the Analysis of River Systems:
A tutorial. CSIRO Technical Report EP111313. 42p.}

\cite{Theobald D.M., Norman J.B., Peterson E.E., Ferraz S., Wade A., and Sherburne M.R. 
(2006) Functional Linkage of Water Basins and Streams (FLoWS) v1 User's Guide: ArcGIS 
tools for Network-based analysis of freshwater ecosystems. Natural Resource Ecology Lab, 
Colorado State University, Fort Collins, USA.}

}
