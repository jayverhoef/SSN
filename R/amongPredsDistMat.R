

amongPredsDistMat <- function(ssn, pids, pred.num, bin.table){

    ##ssn = SpatialStreamNetwork object
    ## pids = list of pid values for prediction sites
    ## pred.num = Index value for prediction dataset in ssn
    ## bin.table = binaryID table for the network

    pred.site.no <- length(pids)
    ind.pids <- rownames(ssn@predpoints@SSNPoints[[pred.num]]@network.point.coords) %in%
        pids

    among_distance_matrix <- matrix(NA, nrow = pred.site.no, ncol = pred.site.no)
    diag(among_distance_matrix)<- 0
    rownames(among_distance_matrix) <- pids
    colnames(among_distance_matrix) <- pids

    locID.pid.data <- attributes(ssn@predpoints@SSNPoints[[pred.num]]@network.point.coords)$locID
    pid.data <- as.data.frame(cbind(as.numeric(rownames(ssn@predpoints@SSNPoints[[pred.num]]@network.point.coords[ind.pids,])),
        as.numeric(levels(ssn@predpoints@SSNPoints[[pred.num]]@network.point.coords$SegmentID[ind.pids]))[ssn@predpoints@SSNPoints[[pred.num]]@network.point.coords$SegmentID[ind.pids]],
        locID.pid.data[ind.pids], ssn@predpoints@SSNPoints[[pred.num]]@network.point.coords$DistanceUpstream[ind.pids]))

    colnames(pid.data)<- c("pid","rid", "locID", "upDist")
    pid.data <- pid.data[order(pid.data$pid),]

    ## Need bin.table

    pid.data$binaryID <- bin.table$binaryID[match(pid.data$rid, bin.table$rid)]
    pid.data <- pid.data[order(pid.data[,"pid"]),]
    rownames(pid.data) <- pid.data$pid

    ##locID values can be repeated, in which case they have the same distance data.
    locID.old <- -1
    for(b in 1:pred.site.no){
       locID.b <- pid.data[b, "locID"]
       upDist.b <- pid.data[b, "upDist"]
       pid.b <- pid.data[b, "pid"]

       if(locID.b != locID.old) {
           junk <- get.rid.fc(pid.data[,"binaryID"], pid.data$binaryID[b])
           truncated.binaryIDs <- data.frame(pid=pid.data[,"pid"], junk, stringsAsFactors = FALSE)
           truncated.binaryIDs$fc <- as.logical(truncated.binaryIDs$fc)
           truncated.binaryIDs$junc.rid <- bin.table$rid[match(truncated.binaryIDs$binaryID, bin.table$binaryID)]

           truncated.binaryIDs$juncDist <- ssn@data$upDist[match(truncated.binaryIDs$junc.rid, ssn@data$rid)]
           truncated.binaryIDs$upDist.j <- pid.data$upDist[match(truncated.binaryIDs$pid, pid.data$pid)]
           ind.fc<-truncated.binaryIDs$fc==1
           dist.preds <- ifelse(ind.fc, upDist.b - truncated.binaryIDs$upDist.j, upDist.b - truncated.binaryIDs$juncDist)
           among_distance_matrix[,paste(pid.b)] <- ifelse(dist.preds<0, 0, dist.preds)
       }
   }
    among_distance_matrix
}
