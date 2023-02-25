\name{MiddleFork04.ssn}
\alias{MiddleFork04.ssn}
\docType{data}
\title{
MiddleFork04.ssn data folder
}
\description{
  The MiddleFork04.ssn data folder contains the spatial, attribute, and topological
  information needed to construct a spatial stream network object using the SSN
  package.
}
\details{
  \preformatted{
  The MiddleFork04.ssn folder contains five spatial datasets:
    edges:    polyline shapefile representing the stream network
    sites:    point shapefile representing the observed site locations
    CapeHorn: point shapefile representing prediction site locations on the Cape
              Horn River
    Knapp:    point shapefile representing prediction site locations on the Knapp
              River
    pred1km:  point shapefile representing the prediction site locations

  There is also 1 text file, netID1.txt, which contain the topological information
  for the stream network in the Middle Fork dataset.

  Attribute data is also stored within each of the spatial datasets. The column
  names are defined as follows:

  edges:
    COMID:	Common identifier of an NHD feature or relationship
    GNIS_ID:	Geographic Names Information System identifier
    GNIS_Name: 	Feature name as found in the Geographic Names Information System
    LENGTHKM:	Length (km)
    REACHCODE:	Unique identifier for a reach. The first 8 digits contain
                the identfier for the HUC8 and the last 6 digits are a
                unique within-HUC8 identifier for the reach
    FCODE:	Numeric code that contains the feature type and it's attributes
    		as found in the NHDFCode lookup table
    CUMdrainAG: Cumulative drainage area (km2) for the lowermost location on the
    		edge
    AREAWTMAP: 	Area weighted mean annual precipitation (mm) at the lowermost
    		location on the edge
    SLOPE:	Slope of the edge (cm/cm)
    h2oAreaKm2: Watershed area (km2) for the lowermost location on the line segment
    rid:        Reach identifier
    areaPI:     Segment proportional influence value, calculated using watershed
                area (h2oAreaKm2)
    afvArea:	Additive function value, calculated using areaPI
    upDist:     Distance from the stream outlet (most downstream location in the
                the stream network) to the uppermost location on the line segment
    netID:	Network identifier
    Length:     Length of line segment (m)


  sites:
    STREAMNAME:	Stream name
    COMID:	Common identifier of an NHD feature or relationship
    CUMDRAINAG:	Cumulative drainage area (km2)
    AREAWTMAP:  Area weighted mean annual precipitation (mm) at lowermost location
  		on the line segment where the site resides
    SLOPE:	Slope of the line segment (cm/cm) where the site resides
    ELEV_DEM:	Elevation at the site based on a 30m DEM
    Deployment:	Unique identifier of the site by year of deployment
    NumberOfDa:	Number of days sampled between the dates of July 15 and August 31.
  		Maximum value is 48.
    Source:	Source of the data - relates to the ID field of the source table
    Summer_mn:	Overall summer mean termperature of the deployment
    MaxOver20:	Binary variable: 1 represents the maximum summer temperature was
    		greater than 20C and 0 indicates that it was less than 20C
    C16:	the number of times daily stream temperature exceeded 16C
    C20:	the number of times daily stream temperature exceeded 20C
    C24:	the number of times daily stream temperature exceeded 24C
    FlowCMS:	Average stream flow (cubic meters per sec) for August, by year,
    		from 1950-2010 across 9 USGS gauges in the region
    AirMEANc: 	Average mean air temperature (C) from July 15 - August 31, from
    		1980-2009 across 10 COOP air stations within the domain
    AirMWMTc:	Average maximum air temperature (C) from July 15 - August 31, from
  		1980-2009 across 10 COOP air stations within the domain. MWMT =
  		maximum 7-day moving average of the maximum daily temperature
  		(i.e. maximum of all the 7-day maximums)
    NEAR_FID:	the FID of the nearest edge feature
    NEAR_DIST:	the distance between the original site coordinates and the nearest
  	   	edge
    NEAR_X:	x coordinate
    NEAR_Y:	y coordinate
    NEAR_ANGLE:	the angle between the original site location and the nearest
  		point on the closest edge
    rid:	Reach identifier of the edge the site resides on
    ratio:	Site ratio value; provides the proportional distance along the
    		edge to the site location
    upDist:	Distance upstream from the stream outlet (m)
    afvArea:	Additive function value calculated using waterhsed area (h2oAreaKm2)
    locID:      Location identifier
    netID:      Stream network identifier
    pid:        Point identifier


  pred1km, CapeHorn, & Knapp:
    COMID:	Common identifier of an NHD feature or relationship
    CUMDRAINAG:	Cumulative drainage area (km2)
    AREAWTMAP:  Area weighted mean annual precipitation (mm) at lowermost location
  		on the line segment where the site resides
    SLOPE:	Slope of the line segment (cm/cm) where the site resides
    ELEV_DEM:	Elevation at the site based on a 30m DEM
    NEAR_FID:	the FID of the nearest edge feature
    NEAR_DIST:	the distance between the original site coordinates and the nearest
  	   	edge
    NEAR_X:	x coordinate
    NEAR_Y:	y coordinate
    NEAR_ANGLE:	the angle between the original site location and the nearest
  		point on the closest edge
    rid:	Reach identifier of the edge the site resides on
    ratio:	Site ratio value; provides the proportional distance along the
  		edge to the site location
    upDist:	Distance upstream from the stream outlet (m)
    afvArea:	Additive function value calculated using watershed area (h2oAreaKm2)
    locID:      Location identifier
    netID:      Stream network identifier
    pid:        Point identifier
    FlowCMS:	Average stream flow (cubic meters per sec) for August, by year,
  		from 1950-2010 across 9 USGS gauges in the region
    AirMEANc: 	Average mean air temperature (C) from July 15 - August 31, from
  		1980-2009 across 10 COOP air stations within the domain
    AirMWMTc:	Average maximum air temperature (C) from July 15 - August 31, from
  		1980-2009 across 10 COOP air stations within the domain. MWMT =
  		maximum 7-day moving average of the maximum daily temperature
  		(i.e. maximum of all the 7-day maximums)


}
}
\source{
  edges: modified version of the National Hydrography Dataset (http://nhd.usgs.gov/)
  sites, pred1km, CapeHorn, & Knapp: U.S. Forest Service, unpublished data.

}

\examples{
	library(SSN)
	#for examples, copy MiddleFork04.ssn directory to R's temporary directory
	copyLSN2temp()
	mf04p <- importSSN(paste0(tempdir(),'/MiddleFork04.ssn'), predpts = "pred1km",
		o.write = TRUE)
	mf04p <- importPredpts(target = mf04p, predpts = "CapeHorn", obj.type = "ssn")
	mf04p <- importPredpts(target = mf04p, predpts = "Knapp", obj.type = "ssn")
	names(mf04p)
}
