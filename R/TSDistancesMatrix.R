TSDistancesMatrix=function(TSMatrix,method='euclidean'){
  AnzTs=ncol(TSMatrix)
  #requireRpackage('TSdist')
  Di=matrix(NaN,AnzTs,AnzTs)
if(sum(!is.finite(TSMatrix))>0)
  warning("Non finite values found. Please use imputeTS::na.interpolation(TSMatrix[,i],option = 'spline') for relevant ith TS or another approach.")

if(!is.matrix(TSMatrix)){
  warning('TSmatrix is not a matrix, Trying to transform..')
  TSMatrix=as.matrix(TSMatrix)
}
  
  for(i in 1:AnzTs){
    for(j in 1:AnzTs){
      if(i < j){
        Di[i,j] = TSdist::TSDistances(TSMatrix[,i],TSMatrix[,j],distance=method)
      } 
    }  #for j
  }  #for i
  
diag(Di)=0
Di[lower.tri(Di)]=Di[upper.tri(Di)]

return(Di)
}
