## Get Confusion Matrix

getConfusionMat <- function(x, threshold = 0.5) {

    ## Check that class of x is data.frame or influenceSSN
    if(class(x) != "influenceSSN") {
        stop("x must be a an influenceSSN object, which is
              created by the residuals function.")}
    x1 <- getSSNdata.frame(x$ssn.object)
    pred.name <- "_CrossValPred_"

    logit.threshold <- log(threshold/(1-threshold))

    x1[, "class"] <- NA

    ind <- x1[,pred.name] >= logit.threshold
    x1[ind, "class"]<- 1
    x1[!ind, "class"] <- 0

    obs.name <- x$args$zcol

    class.00 <- sum((x1[,obs.name] == 0) & (x1$class == 0))
    class.01 <- sum((x1[,obs.name] == 0) & (x1$class == 1))
    class.10 <- sum((x1[,obs.name] == 1) & (x1$class == 0))
    class.11 <- sum((x1[,obs.name] == 1) & (x1$class == 1))

    confMat <- matrix(NA, nrow = 2, ncol = 2)
    rownames(confMat) <- c("Obs.0", "Obs.1")
    colnames(confMat) <- c("Preds.0", "Preds.1")

    confMat[1,1]<- class.00
    confMat[1,2]<- class.01
    confMat[2,1] <- class.10
    confMat[2,2] <- class.11

    print(confMat)

}

