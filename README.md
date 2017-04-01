[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)

[![minimal R version](https://img.shields.io/badge/R%3E%3D-3.3.3-6666ff.svg)](https://cran.r-project.org/) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/kotzeb0912)](https://cran.r-project.org/package=kotzeb0912) [![packageversion](https://img.shields.io/badge/Package%20version-1.1.10-orange.svg?style=flat-square)](commits/master)

[![Last-changedate](https://img.shields.io/badge/last%20change-2017--04--01-yellowgreen.svg)](/commits/master)

# SSN 
## Spatial Modeling on Stream Networks 

#### Jay M. Ver Hoef<sup>a</sup> and Erin E. Peterson<sup>b</sup>

#### <sup>a</sup>NOAA Fisheries (NMFS) Alaska Fisheries Science Center, and 
#### <sup>b</sup>Queensland University of Technology


Executive Summary
-----------------

Spatial statistical modeling and prediction for data on stream networks, including models based on in-stream distance.  Models are created using moving average constructions. Spatial linear models, including explanatory variables, can be fit with (restricted) maximum likelihood.  Mapping and other graphical functions are included.

Installation
------------

Installation of this R data package is done through the `devtools::install_github()` function or by downloading the [source package from the latest release](https://github.com/jayverhoef/SSN).  Most likely, you can just install from within R.  After starting R, first make sure that you have the devtools package.  

```
install.packages("devtools")
```

Then, use the code below

```
library("devtools")
install_github("jayverhoef/SSN")
```

