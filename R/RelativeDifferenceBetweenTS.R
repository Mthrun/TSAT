RelativeDifferenceBetweenTS=function(TS1,TS2,na.rm=TRUE,Silent=FALSE){
    if(isTRUE(na.rm)){
      requireNamespace('imputeTS')
      if(sum(!is.finite(TS1))>0)
        TS1=imputeTS::na.interpolation(TS1,option = 'linear')
      if(sum(!is.finite(TS2))>0)
        TS2=imputeTS::na.interpolation(TS2,option = 'linear')
    }else{
      if(sum(!is.finite(TS1))>0||sum(!is.finite(TS2))>0) stop('Infinitive values in time series. Please use na.rm=T.')
    }
  
  neg_ind1 = which(TS1<0)
  if (length(neg_ind1) > 0) {
    if (isTRUE(na.rm)){
      TS1[neg_ind1]=0
    }
    if(isFALSE(Silent))
      message(
        "RelativeDifferenceBetweenTS works only for positive values correctly but negative values were found in TS1. These cases are set to zero."
      )
  }else{
    if(isFALSE(Silent))
      warning(
        "RelativeDifferenceBetweenTS works only for positive values correctly but negative values were found in TS1."
      )
  }
  
  
  neg_ind2 = which(TS2<0)
  if (length(neg_ind2) > 0) {
    if (isTRUE(na.rm)){
      TS2[neg_ind2]=0
    }
    if(isFALSE(Silent))
      message(
        "RelativeDifferenceBetweenTS works only for positive values correctly but negative values were found in TS1. These cases are set to zero."
      )
  }else{
    if(isFALSE(Silent))
      warning(
        "RelativeDifferenceBetweenTS works only for positive values correctly but negative values were found in TS1."
      )
  }
  
    deno <- TS1 + TS2
    nZeroInd <- deno == 0
    if (sum(nZeroInd) > 0) {
      deno[nZeroInd] = 1
    }
    relDiff = 2 * (TS1 - TS2)/deno
    return(relDiff)

}