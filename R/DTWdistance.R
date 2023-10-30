# dist = DTWdistance(Data)
#
# DESCRIPTION
# Dynamic Time Warping distance based an Non-Euclidean pairwise distance
#
# INPUT
# Data                [1:n,1:d] matrix with d Timeseries of the length n
# DistanceFunction    A function which calculates the pairwise distances 
#                     in the same way as proxy::dist
# ...                 Further Arguments for DistanceFunction
#
# OUTPUT
# [1:d,1:d] symmetric matrix of distances
#
# DETAILS
# Currently only implemented for time series of same length. Distances in DistanceFunction 
# should be calculated the same ways as in a two-dimensional data matrix with the output
# of the function being object of class dist.

DTWdistance=function(Data,DistanceFunction=proxy::dist,...){
  requireNamespace('dtw')
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

#alternative, vieleicht schneller: IncDTW::dtw_dismat()