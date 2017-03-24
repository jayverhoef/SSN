\name{SpatialStreamNetwork-class}
\Rdversion{1.1}
\docType{class}
\alias{SpatialStreamNetwork-class}
\title{Class "SpatialStreamNetwork"}
\description{
A class that holds spatial features (points and lines), attributes, and
topological information for stream networks.
}
\section{Objects from the Class}{
SpatialStreamNetwork objects can be created by calls in the form \code{importSSN(x)}.
}
\section{Slots}{
  \describe{
    \item{\code{network.line.coords}:}{
      Object of class \code{"data.frame"}; columns include \code{NetworkID} (a
      factor identifying individual networks that are assumed independent for
      modeling purposes), \code{SegmentID} (a unique identifier for each stream
      segment), and \code{DistanceUpstream} (the cumulative distance from the
      network outlet, the most downstream point on a network, to the uppermost
      point of that stream segment). Row names are set to \code{SegmentID}
      values.
      }
    \item{\code{obspoints}:}{
      Object of class \code{"SSNPoints"} with 2 slots
      \preformatted{
          @ SSNPoints: List of SSNPoint objects with 5 slots
              @ network.point.coords: object of class "data.frame". Row names
                represent point identifiers (pid) stored in the point.data
                data.frame.
                  $ NetworkID: factor identifying the NetworkID of that point
                  $ SegmentID: factor identifying the unique stream segment of that point
                  $ DistanceUpstream: numeric value representing the cumulative
                    distance from the network outlet, the most downstream point
                    on a network, to that point
              @ point.coords: numeric matrix or "data.frame" with x- and y-
                coordinates (each row is a point); row names represent point
                identifiers (pid) stored in the point.data data.frame.
              @ point.data: object of class "data.frame"; the number of rows in
                data should equal the number of points in the
                network.point.coords object; row names are set to the pid
                attribute.
              @ points.bbox: Object of class "matrix"; see Spatial-class
              @ proj4string: Object of class "CRS"; see CRS-class
          @ ID: character string representing the name of the observation points
	    }}
    \item{\code{predpoints}:}{
      Object of class \code{"SSNPoints"}. See description of object format under
      slot obspoints }
    \item{\code{path}:}{
      Object of class \code{"character"} representing the file location of the
      SSN dataset }
    \item{\code{data}:}{
      Object of class \code{"data.frame"}. The number of rows in data should
      equal the number of lines in the lines object. Row names correspond to
      \code{SegmentID} values}
    \item{\code{lines}:}{
      Object of class \code{"list"}; list members are all of class \link[sp:Lines-class]{Lines}.
      Each list item represents a polyline segment and the number of list items
      should be equal to the number of rows in the \code{network.line.coords}
      object
      }
    \item{\code{bbox}:}{
      Object of class \code{"matrix"}; see \link[sp:Spatial-class]{Spatial}
      }
    \item{\code{proj4string}:}{
      Object of class \code{"CRS"}; see \code{CRS}
      }
  }
}
\section{Extends}{
Class \link[sp:SpatialLinesDataFrame]{SpatialLinesDataFrame}, directly.
Class \link[sp:SpatialLines-class]{SpatialLines-class}, by class "SpatialLinesDataFrame", distance 2.
Class \link[sp:Spatial]{Spatial}, by class "SpatialLinesDataFrame", distance3.
}

\seealso{
  \link[sp:Line-class]{Line-class}, \link[sp:Lines-class]{Lines-class}
}
