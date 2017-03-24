createDistMat <-
function(ssn, predpts = NULL, o.write = FALSE, amongpreds = FALSE) {

  ## Check that arguments are valid ------------------------------------------
  ##start.time <- Sys.time()
  if(amongpreds && (missing(predpts) || is.null(predpts)))
  {
	stop("A named collection of prediction points must be specified via the predpts option when amongpreds is TRUE")
  }
  ##Check to see whether distance folder exists...
  if (!file.exists(file.path(ssn@path, "distance"))) {
    dir.create(file.path(ssn@path, "distance"))
  }
  ##And then whether an observation folder exists
  if (!file.exists(file.path(ssn@path, "distance", "obs"))) {
    dir.create(file.path(ssn@path, "distance", "obs"))
  }

  ## And then whether prediction folder exists
  if (!is.null(predpts)) {
    if(!file.exists(file.path(ssn@path, "distance", predpts))) {
      dir.create(file.path(ssn@path, "distance", predpts))
    }
    count <- 0
    if(length(ssn@predpoints@ID) > 0) {
	for (m in 1:length(ssn@predpoints@ID)) {
	    if (ssn@predpoints@ID[m] == predpts) {
	         pred.num <- m
                 count <- count + 1}
	}
    }

    if (count==0) {
      stop(predpts, " does not exist in SSN")}

    if (count > 1) {
      stop("SSN contains more than one copy of ", predpts)}

    ssn@predpoints@SSNPoints[[pred.num]]@point.data$netID<- as.factor(ssn@predpoints@SSNPoints[[pred.num]]@point.data$netID)
  }

  if (is.null(predpts)) {
      pred.num <- 0}

  ##Initialise binaryID.db-------------------------------------------------
  if (file.exists(file.path(ssn@path,"binaryID.db")) == FALSE)
    stop("binaryID.db is missing from ssn object")

  driver <- RSQLite::SQLite()
  connect.name <- file.path(ssn@path,"binaryID.db")

  connect <- dbConnect(SQLite(), connect.name)

  ## close sqlite connection upon function exit
  on.exit({
      dbDisconnect(connect)
  })

  if (file.exists(file.path(ssn@path, "binaryID.db")) == FALSE)
      stop("binaryID.db is missing from ssn object")

  ssn@obspoints@SSNPoints[[1]]@network.point.coords$NetworkID<-
      as.factor(ssn@obspoints@SSNPoints[[1]]@network.point.coords$NetworkID)
  net.count <- length(levels(ssn@network.line.coords$NetworkID))
  warned.overwrite <- FALSE

## For each network --------------------------------------------------------
  for (i in 1:net.count) {
      net.num <- levels(ssn@network.line.coords$NetworkID)[i]
      ind.obs <- ssn@obspoints@SSNPoints[[1]]@network.point.coords$NetworkID == as.numeric(net.num)

      ## figure out how many observed and prediction sites there are in the network
      site.no <- nrow(ssn@obspoints@SSNPoints[[1]]@network.point.coords[ind.obs,])

      if (pred.num > 0) {
          ind.preds <- ssn@predpoints@SSNPoints[[pred.num]]@network.point.coords$NetworkID == as.numeric(net.num)
          pred.site.no <- nrow(ssn@predpoints@SSNPoints[[pred.num]]@network.point.coords[ind.preds,])
      } else { pred.site.no <-0 }

      if (site.no > 0) {

          ## get sorted pids to use as dim names
	  obs.pids<- sort(as.numeric(rownames(ssn@obspoints@SSNPoints[[1]]@network.point.coords[ind.obs,])))

          ## If predpts !is.null---------------------------------------------
	  if(!is.null(predpts)){
                ## Get pred pids
		pred.pids<- sort(as.numeric(rownames(ssn@predpoints@SSNPoints[[pred.num]]@network.point.coords[ind.preds,])))
                ## create empty distance matrices for obs-preds
		if(pred.site.no > 0){
		    ## create o x p distance matrix full of NA
                    current_distance_matrix_a <- matrix(NA, nrow = site.no, ncol = pred.site.no,
                    dimnames=list(obs.pids, pred.pids))

                    ## create p x o distance matrix full of NA
                    current_distance_matrix_b <- matrix(NA, nrow = pred.site.no, ncol = site.no,
                    dimnames=list(pred.pids, obs.pids))
		}
	  }

          net.name <- paste("net", net.num, sep = "")
          workspace.name.a <- paste("dist.net", net.num, ".a.RData", sep = "")
	  workspace.name.b <- paste("dist.net", net.num, ".b.RData", sep = "")
          ## Extract binaryID table
          bin.table <- dbReadTable(connect, net.name)

  	  ##Create n x n distance matrix full of NA
	  ##If distance matrix table already exists within the database and
          ## o.write == TRUE, exit function
	  workspace.name <- paste("dist.net", net.num, ".RData", sep = "")

	  if(!o.write) {
		exists <- file.exists(file.path(ssn@path, "distance", "obs", workspace.name))
		if (!missing(predpts) && !is.null(predpts))
		{
                    exists <- c(exists, file.exists(file.path(ssn@path,
                                "distance", predpts, workspace.name.a)),
                                file.exists(file.path(ssn@path, "distance",
                                predpts, workspace.name.b)))
		}
		##If they're all already there, warn but continue
		if(all(exists))
		{
			if(!warned.overwrite) {
			    warned.overwrite <- TRUE
			    cat("Distance matrices already existed while o.write was set to FALSE. Not overwriting existing matrices\n")}
			next
		}
		##if some exist but some don't that's an error
		else if(any(exists) && any(!exists)) {
			stop("o.write was set to FALSE and some (but not all) distance matrices already existed")}
	  }

          ## Create empty obs distance matrix
	  current_distance_matrix <- matrix(NA, nrow = site.no, ncol = site.no,dimnames = list(obs.pids, obs.pids))
	  diag(current_distance_matrix)<- 0

          rownames(current_distance_matrix) <- obs.pids
	  colnames(current_distance_matrix) <- obs.pids

          locID.obi <- attributes(ssn@obspoints@SSNPoints[[1]]@network.point.coords[ind.obs,])$locID
          ## Create data.frame for obs with columns pid, rid, locID
          ob.i <- as.data.frame(cbind(as.numeric(rownames(ssn@obspoints@SSNPoints[[1]]@network.point.coords[ind.obs,])),
                        as.numeric(levels(ssn@obspoints@SSNPoints[[1]]@network.point.coords$SegmentID[ind.obs]))[ssn@obspoints@SSNPoints[[1]]@network.point.coords$SegmentID[ind.obs]],
			locID.obi[ind.obs]))

          colnames(ob.i)<- c("pid","rid","locID")
          ob.i$locID <- as.factor(ob.i$locID)

          ob.i$binaryID <- bin.table$binaryID[match(ob.i$rid, bin.table$rid)]
          ob.i <-ob.i[order(ob.i[,"pid"]),]
          rownames(ob.i)<- ob.i$pid
          ob.i_by_locID <- ob.i[order(ob.i[,"locID"]),]
          ob.i_by_locID$pid <- as.numeric(ob.i_by_locID$pid)
          ob.i_by_locID$locID <- as.numeric(ob.i_by_locID$locID)
          ob.j_reordering <- order(ob.i_by_locID$pid)


          locID.old <- -1
          ind.dup <- !duplicated(ob.i_by_locID$locID)

          ##Calculate distances between observed sites
          for (j in 1:nrow(ob.i)) {

            pid.i <- ob.i[j,"pid"]
            locID.i <- ob.i[j, "locID"]

            if (locID.i != locID.old) {

              junk <- get.rid.fc(ob.i_by_locID[ind.dup,"binaryID"], ob.i$binaryID[j])

              ob.j <- getObsRelationshipsDF(ssn, pid.i,  junk, ind.dup, ob.i,
                                      ob.i_by_locID,bin.table)
	      upDist.i <- ssn@obspoints@SSNPoints[[1]]@network.point.coords[paste(pid.i),
                                       "DistanceUpstream"]

              ob.j <-ob.j[ob.j_reordering,]

              ##obs fills in by column because it's working between obs to obs.
              ind.fc<-ob.j$fc==1

              dist.obs <- ifelse(ind.fc, upDist.i - ob.j$upDist.j,
                                 upDist.i - ob.j$juncDist)
              current_distance_matrix[,paste(pid.i)] <- ifelse(dist.obs<0, 0, dist.obs)

          } else {
              current_distance_matrix[,paste(pid.i)]<-
                  current_distance_matrix[,paste(pid.old)]
          }

    #####Calculate distance from observed site to pred sites
          if (locID.i != locID.old) {

            if (!is.null(predpts) && pred.site.no > 0) {

              ob.j <- getPredRelationshipsDF(ssn, pred.num, ind.preds, bin.table, ob.i, j)
              ob.j <-ob.j[order(ob.j[,"pid"]),]

              ind.fc<-ob.j$fc==1
              dist.a <- ifelse(ind.fc, ob.j$upDist.j-upDist.i, ob.j$upDist.j - ob.j$juncDist)
              current_distance_matrix_a[paste(pid.i), ] <- ifelse(dist.a<0, 0, dist.a)

              dist.b <- ifelse(ind.fc, upDist.i - ob.j$upDist.j, upDist.i - ob.j$juncDist)
              current_distance_matrix_b[, paste(pid.i)] <- ifelse(dist.b<0, 0, dist.b)
            }
          } else {
            ## add column to pred sites
            if (!is.null(predpts) && pred.site.no > 0) {
                current_distance_matrix_a[paste(pid.i),]<-
                    current_distance_matrix_a[paste(pid.old),]
                current_distance_matrix_b[,paste(pid.i)]<-
                    current_distance_matrix_b[,paste(pid.old)]}
          }

          pid.old <- pid.i
          locID.old <- locID.i
        }

      ## Write distance matrices to distance folder --------------------------------------
      ## Observed distance matrix
      file_handle = file(file.path(ssn@path, "distance", "obs", workspace.name), open="wb")
      serialize(current_distance_matrix, file_handle, ascii=FALSE)
      close(file_handle)


      if(pred.site.no > 0) {
      	##save obs-pred and pred-obs distance matrices
      	file_handle = file(file.path(ssn@path, "distance", predpts, workspace.name.a), open="wb")
      	serialize(current_distance_matrix_a, file_handle, ascii=FALSE)
      	close(file_handle)

      	file_handle = file(file.path(ssn@path, "distance", predpts, workspace.name.b), open="wb")
      	serialize(current_distance_matrix_b, file_handle, ascii=FALSE)
      	close(file_handle)
    }
      }

      ## Calculate distances between prediction sites ------------------------------------
      if (amongpreds & pred.site.no > 0) {
              ##Create distance matrix full of NAs. If the file already exists
              ## and overwrite == FALSE, stop with an error
             workspace.name <- paste("dist.net", net.num, ".RData", sep = "")
             pred.pids<- sort(as.numeric(rownames(ssn@predpoints@SSNPoints[[pred.num]]@network.point.coords[ind.preds,])))
             net.name <- paste("net", net.num, sep = "")
             bin.table <- dbReadTable(connect, net.name)

             among_distance_matrix <- amongPredsDistMat(ssn, pred.pids, pred.num, bin.table)
             ## Write preds-preds distance matrix to distance folder
             file_handle = file(file.path(ssn@path, "distance", predpts, workspace.name),
                 open="wb")
	     serialize(among_distance_matrix, file_handle, ascii=FALSE)
	     close(file_handle)
      }
}}

