
getPredRelationshipsDF<- function(ssn, num, ind, bin, ob, j ) {
    ## ssn = SpatialStreamNetwork object
    ## num = index of predpts in SSN
    ## ind = index for pred values that lie on this network
    ## bin = binaryID table for this network
    ## ob =  data.frame with pid, rid, locID, and binaryID for sites on network
    ##         ordered by pid (ob.i)
    ## j = row j of data.frame ob


    pred.tmp <- as.data.frame(cbind(as.numeric(rownames(ssn@predpoints@SSNPoints[[num]]@network.point.coords[ind,])),
        as.numeric(levels(ssn@predpoints@SSNPoints[[num]]@network.point.coords$SegmentID[ind]))[ssn@predpoints@SSNPoints[[num]]@network.point.coords$SegmentID[ind]]))

    colnames(pred.tmp)<- c("pid","rid")

    pred.tmp$binaryID <- bin$binaryID[match(pred.tmp$rid, bin$rid)]
    pred.tmp <-pred.tmp[order(pred.tmp[,"pid"]),]
    rownames(pred.tmp) <- pred.tmp$pid

    junk <- get.rid.fc(pred.tmp[,"binaryID"], ob$binaryID[j])
    ob.j <- data.frame(pred.tmp["pid"], junk, stringsAsFactors = FALSE)

    ob.j$pid <- as.numeric(ob.j$pid)
    ob.j$fc <- as.logical(ob.j$fc)

    ob.j$junc.rid <- bin$rid[match(ob.j$binaryID, bin$binaryID)]
    ob.j$juncDist <- ssn@network.line.coords$DistanceUpstream[match(ob.j$junc.rid, ssn@network.line.coords$SegmentID)]
    ob.j$upDist.j <- ssn@predpoints@SSNPoints[[num]]@network.point.coords$DistanceUpstream[
                                                                                           match(ob.j$pid, as.numeric(rownames(ssn@predpoints@SSNPoints[[num]]@network.point.coords)))]
    ob.j
}
