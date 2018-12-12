DTWdistance=function(Data,DistanceFunction=proxy::dist,...){

  d=ncol(Data)
  Distance=matrix(NaN,d,d)
  diag(Distance)=0
  for(i in 1:d){
    for(j in 1:d){
      #if(i!=j){
        if(i>j){
          x=Data[,i]
          y=Data[,j]
          cxdist <- DistanceFunction(x, y,...)
          Distance[i,j]=dtw::dtw(cxdist)$distance
        }
      #}
    }
  }
  
  Distance[upper.tri(Distance)] <- 0
  if(sum(is.nan(Distance))>0) warning('Distance calculation somehow got wrong')
  
  if(sum(!is.finite(Distance))>0) warning('Distance calculation between at least two values resulted in an infinite value.')
  
  Distance <- Distance + t(Distance)
  
return(Distance)
}