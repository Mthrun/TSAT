RelativeDifference=function(TS1,TS2,na.rm=TRUE){
    if(na.rm){
      requireNamespace('imputeTS')
      if(sum(!is.finite(TS1))>0)
        TS1=imputeTS::na.interpolation(TS1,option = 'linear')
      if(sum(!is.finite(TS2))>0)
        TS2=imputeTS::na.interpolation(TS2,option = 'linear')
    }else{
      if(sum(!is.finite(TS1))>0||sum(!is.finite(TS2))>0) stop('Infinitive values in time series. Please use na.rm=T.')
    }
    deno <- TS1 + TS2
    nZeroInd <- deno == 0
    if (sum(nZeroInd) > 0) {
      deno[nZeroInd] = 1
    }
    relDiff = 2 * (TS1 - TS2)/deno
    return(relDiff)

}