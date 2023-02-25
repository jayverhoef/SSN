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
Version: \tab 1.1.12\cr
Date: \tab 2018-01-24\cr
License: \tab GPL-2 \cr
LazyLoad: \tab yes\cr
}


The SSN package provides tools to fit generalized linear models with spatial autocorrelation for stream network data.  SSN uses normal likelihood methods (including REML) and quasi-likelihood for Poisson and Binomial families. The spatial formulation was originally described in Ver Hoef, Peterson, and Theobald (2006), with more details given by Ver Hoef and Peterson (2010) and Peterson and Ver Hoef (2010). The spatial data must be formatted in a geographic information system (GIS) prior to importing it into R. A custom  ArcGIS toolbox has been provided to format the data:  Spatial Tools for the Analysis of River Systems (STARS) toolset (Peterson and Ver Hoef, 2014), and the SSN package is fully described in Ver Hoef, Peterson, Clifford, and Shah (2014).
}
\author{
Jay Ver Hoef and Erin Peterson \email{support@SpatialStreamNetworks.com}
}
\references{

\cite{Ver Hoef, J. M., Peterson, E. E. and Theobald, D. (2006) Spatial Statistical Models that
Use Flow and Stream Distance. \emph{Environmental and Ecological Statistics} \bold{13}, 449--464.}

\cite{Ver Hoef, J. M. and Peterson, E. E. (2010) A Moving Average Approach for 
Spatial Statistical Models of Stream Networks (with Discussion). \emph{Journal of the American Statistical Association} \bold{105}, 6--18. DOI: 10.1198/jasa.2009.ap08248.  Rejoinder pgs. 22--24.}

\cite{Peterson, E. E. and Ver Hoef, J. M. (2010) A Mixed-Model Moving-Average 
Approach to Geostatistical Modeling in Stream Networks. \emph{Ecology} 
\bold{91(3)}, 644--651.}

\cite{Peterson, E. E. and Ver Hoef, J. M. 2014. STARS: An ArcGIS Toolset Used to
Calculate the Spatial Information Needed to Fit Spatial Statistical Models to Stream
Network Data . \emph{Journal of Statistical Software} \bold{56(2)}: 1--17.}

\cite{Ver Hoef, J. M., Peterson, E. E., Clifford, D. and Shah, R. (2014) SSN: An R Package
for Spatial Statistical Modeling on Stream Networks. \emph{Journal of Statistical Software}
\bold{56(3)}: 1--45.}

}
