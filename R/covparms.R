covparms <-
function(object) {
  if(class(object)[[1]] != "glmssn") return("Not a glmssn object")
  summary(object)$covariance.parameter.estimates
}

