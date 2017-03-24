

getObsRelationshipsDF <- function(ssn, pid, junk, ind, ob, ob_by_locID, bin) {

    ## ssn = SpatialStreamNetwork object
    ## pid = integer pid value of interest
    ## junk = ox2 data.frame with columns fc (logical) and binaryID
    ## ind = vector indicator saying whether it is a duplicate
    ## ob = data.frame with pid, rid, locID, and binaryID for sites on network
    ##      ordered by pid
    ## ob_by_locID = data.frame with pid, rid, locID, and binaryID for sites on network
    ##               ordered by locID
    ## bin = binaryID table

    ## Returns a data.frame that relates all sites to pid.i:
    ## pid: numeric
    ## locID: numeric
    ## fc: logical - is the sute fc with the pid of interest
    ## binaryID: binaryID of the common downstream junction
    ## junc.rid: rid for the common downstream junction
    ## upDist.j: upDist for each site

## Create relationships table

      ob.j.r <- data.frame(ob_by_locID[ind, c("pid", "locID")], junk,
                           stringsAsFactors = FALSE)

      ob.j.r$fc <- as.logical(ob.j.r$fc)
      rownames(ob.j.r)<- ob.j.r$pid

      ## Add column showing rid for common downstream junction (ob.j.r relates all               ##sites to pid)
      ob.j.r$junc.rid <- bin$rid[match(ob.j.r$binaryID, bin$binaryID)]

      ## This doesn't make sense to me...
      reps <- as.numeric(ob_by_locID$locID)
      ob.j <- ob.j.r[reps,]

      ## Create some funky rownames, with extension .fc
      rownames(ob.j) <- paste(rownames(ob), ".fc", sep = "")

        ## Don't know why we're doing this...
        ob.j$pid <- ob_by_locID$pid

      ## juncDist is the upstream distance of the common downstream rid junction
      ob.j$juncDist <- ssn@network.line.coords$DistanceUpstream[match(ob.j$junc.rid, ssn@network.line.coords$SegmentID)]

      ## upDist.j is the upDist for each observed site
      ob.j$upDist.j <- ssn@obspoints@SSNPoints[[1]]@network.point.coords$DistanceUpstream[
                    match(ob.j$pid, as.numeric(rownames(ssn@obspoints@SSNPoints[[1]]@network.point.coords)))]

      ob.j

}
