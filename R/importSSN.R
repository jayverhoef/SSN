importSSN <-
function(filepath, predpts = NULL, o.write = FALSE) {

  old.wd <- getwd()

  Path <- dirname(filepath)
  ssn.obj <- basename(filepath)

  if (Path == ".") { Path = old.wd }

  setwd(paste(Path, "/", ssn.obj, sep= ""))

  # IMPORT SHAPEFILES- these are stored as a SpatialLinesDataFrame and
  #    a SpatialPointsDataFrame
  ## edges <- readOGR(".", "edges",verbose = FALSE, stringsAsFactors = FALSE,
  ##          integer64 = "allow.loss")
  edges<- st_read("edges.shp",quiet=TRUE)

  if (exists("edges")==0) {
    stop("edges.shp is missing from ", Path, " folder")
  }
  ## Check geometry type
  if (sum(st_geometry_type(edges, by_geometry=TRUE)=="LINESTRING") != nrow(edges)){
    stop("edges.shp does not have polyline geometry")
  }
  edges<- as_Spatial(edges)
  rownames(edges@data) <- edges@data[,"rid"]

  ##sites <- readOGR(".", "sites",verbose = FALSE, stringsAsFactors = FALSE,
    ##               integer64 = "allow.loss")
  sites<- st_read("sites.shp", quiet=TRUE)
  ## Check geometry
  if (exists("sites")==0) {
    stop("sites.shp data is missing from ", Path," ssn folder")
  }
  if (sum(st_geometry_type(sites, by_geometry=TRUE)=="POINT")!= nrow(sites)){
    stop("sites.shp does not have point geometry")
  }
  sites<- as_Spatial(sites)
  ord = order(sites@data$pid)
  sites@data <- sites@data[ord,]
  sites@coords <- sites@coords[ord,]
  rownames(sites@data) <- sites@data[,"pid"]
  rownames(sites@coords) <- sites@data[,"pid"]

  sites@data$locID <- as.factor(sites@data$locID)



  # SET NETWORK.LINE.COORDS
  ind1 <- colnames(edges@data)== c("netID")
  ind2 <- colnames(edges@data)== c("rid")
  ind3 <- colnames(edges@data)== c("upDist")

  if (sum(ind1) == 0) {
    stop("netID is missing from streams attribute table")
    }
  if (sum(ind2) == 0) {
    stop("rid is missing from streams attribute table")
    }
  if (sum(ind3) == 0) {
    stop("upDist is missing from streams attribute table")
    }

  if (is.factor(edges@data$netID))  {
    edges@data$netID <- as.character(edges@data$netID)}


  network.line.coords <- data.frame(edges@data$netID, edges@data[,"rid"], edges@data[,"upDist"])
  colnames(network.line.coords)<-c("NetworkID", "SegmentID", "DistanceUpstream")
  network.line.coords <- as.data.frame(network.line.coords)
  row.names(network.line.coords) <- row.names(edges@data)

  network.line.coords[,1] <- as.factor(network.line.coords[,1])
  network.line.coords[,2] <- as.factor(network.line.coords[,2])

  rm(ind1, ind2, ind3)

  # SET NETWORK.POINT.COORDS
  ind1 <- colnames(sites@data)== c("netID")
  ind2 <- colnames(sites@data)== c("rid")
  ind3 <- colnames(sites@data)== c("upDist")

  if (sum(ind1) == 0) {
    stop("netID is missing from sites attribute table")
    }
  if (sum(ind2) == 0) {
    stop("rid is missing from sites attribute table")
    }
  if (sum(ind3) == 0) {
    stop("upDist is missing from sites attribute table")
    }

  if (is.factor(sites@data$netID)) {
    sites@data$netID <- as.character(sites@data$netID) }

  network.point.coords <- data.frame(sites@data[,"netID"], sites@data[,"rid"], sites@data[,"upDist"])
  colnames(network.point.coords)<-c("NetworkID", "SegmentID", "DistanceUpstream")
  network.point.coords <- as.data.frame(network.point.coords)
  row.names(network.point.coords) <- row.names(sites@data)
######### New #################################################################
  attributes(network.point.coords)$locID <- as.numeric(levels(sites@data$locID))[sites@data$locID]

  network.point.coords[,1] <- as.factor(network.point.coords[,1])
  network.point.coords[,2] <- as.factor(network.point.coords[,2])
  network.point.coords[,3] <- as.numeric(network.point.coords[,3])

  rm(ind1, ind2, ind3)

  #Set observed sites as SSNPoint object
  op <- new("SSNPoint",
    network.point.coords = network.point.coords,
    point.coords = sites@coords,
    point.data = sites@data,
    points.bbox = sites@bbox,
    proj4string = sites@proj4string)

  #Create SSNPoints list for input into SSN object
  ops<-new("SSNPoints")
  ops@SSNPoints[[1]]<- op
  ops@ID[[1]]<- "Obs"

  rm(network.point.coords, sites, op)

  #Add prediction points here-----------------------------------------------------
  if (!is.null(predpts)) {
      ## predpoints <- readOGR(".", predpts, verbose = FALSE, stringsAsFactors = FALSE,
      ##      integer64 = "allow.loss")
      predpoints<- st_read(paste0(predpts, ".shp"), quiet=TRUE)
     if (file.exists(paste(predpts,".shp",sep = ""))==0) {
       stop(paste(predpts,".shp data is missing from ", Path, "/", ssn.obj,
                " folder",sep = ""))
     }
     ## Check geometry type
     if (sum(st_geometry_type(predpoints, by_geometry=TRUE)=="POINT")!=nrow(predpoints)){
       stop(paste0(predpts, " does not have point geometry"))
     }
     predpoints<- as_Spatial(predpoints)

      ord = order(predpoints@data$pid)
      predpoints@data <- predpoints@data[ord,]
      predpoints@coords <- predpoints@coords[ord,]

      rownames(predpoints@data) <- predpoints@data[,"pid"]
      rownames(predpoints@coords) <- predpoints@data[,"pid"]
      predpoints@data$locID <- as.factor(predpoints@data$locID)

      ## if (file.exists(paste(predpts,".shp",sep = ""))==0) {
      ##   stop(paste(predpts,".shp data is missing from ", Path, "/", ssn.obj, " folder",sep = ""))
      ## }
      ## if (getinfo.shape(predpts)[[2]] != 1){
      ##   stop(paste(predpts,".shp does not have point geometry", sep = ""))
      ## }

      if (is.factor(predpoints@data$netID)) {
        predpoints@data$netID <- as.character(predpoints@data$netID) }

      network.point.coords <- data.frame(predpoints@data[,"netID"], predpoints@data[,"rid"], predpoints@data[,"upDist"])
      colnames(network.point.coords)<-c("NetworkID", "SegmentID", "DistanceUpstream")
      network.point.coords <- as.data.frame(network.point.coords)
      row.names(network.point.coords) <- row.names(predpoints@data)
#### New #######################################################################
      attributes(network.point.coords)$locID <- as.numeric(levels(predpoints@data$locID))[predpoints@data$locID]

      network.point.coords[,1] <- as.factor(network.point.coords[,1])
      network.point.coords[,2] <- as.factor(network.point.coords[,2])

      # Create SSNPoint object for prediction sites
      pp <- new("SSNPoint",
        network.point.coords = network.point.coords,
        point.coords = predpoints@coords,
        point.data = predpoints@data,
        points.bbox = predpoints@bbox,
        proj4string = predpoints@proj4string)

#        ssn@predpoints@SSNPoints[[1]]<- pp
#        ssn@predpoints@ID[[1]]<- predpts
      pps<-new("SSNPoints")
      pps@SSNPoints[[1]]<- pp
      pps@ID[[1]]<- predpts

      rm(predpoints, pp, network.point.coords)
  } else {
      pps<-new("SSNPoints")}

  # SET SPATIAL STREAM NETWORK OBJECT (SSN)
  ssn <- new("SpatialStreamNetwork", edges,
    network.line.coords = network.line.coords,
    obspoints = ops,
    predpoints = pps,
    path = paste(Path, "/", ssn.obj, sep= ""))

    ssn@obspoints@SSNPoints[[1]]@point.data$netID<- as.factor(ssn@obspoints@SSNPoints[[1]]@point.data$netID)

    if (!is.null(predpts)) {
    ssn@predpoints@SSNPoints[[1]]@point.data$netID<- as.factor(ssn@predpoints@SSNPoints[[1]]@point.data$netID)}

################################
#Added this line
  ssn@data$netID<- as.factor(ssn@data$netID)
  rm(network.line.coords, edges)

################################################################################
  # CREATE BINARY ID DATABASE-----------------------

  createBinaryID(ssn, o.write = o.write)

  setwd(old.wd)

  #end of function
  ssn


}

