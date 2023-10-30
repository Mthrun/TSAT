# distMat=TSDistancesMatrix(TSMatrix,method='euclidean')
#
# Description:
# Generates a Distance Matrix for Time Series data
#
# INPUT
# Time1                   [1:n,1:m] matrix of m different Time Series with equal length n
# method                  Distance measure to be used. It must be one of: 
#                         "euclidean", "manhattan", "minkowski", "infnorm", "ccor", "sts", "dtw", "keogh.lb", "edr", "erp", 
#                         "lcss", "fourier", "tquest", "dissim", "acf", "pacf", "ar.lpc.ceps", "ar.mah", "ar.mah.statistic", 
#                         "ar.mah.pvalue", "ar.pic", "cdm", "cid", "cor", "cort", "wav", "int.per", "per", "mindist.sax", 
#                         "ncd", "pred", "spec.glk", "spec.isd", "spec.llr", "pdc", "frechet"
#
# OUTPUT
# DistanceMatrix[1:m,1:m]
# 
# Details: 
# For long time series of possibly very different lengths, the shape-based methods do not
# give intuitive results, feature- [e.g. fourier] and model-based [e.g. acf] methods  should be considered
#
#
# Author: MCT

TSDistancesMatrix=function(TSMatrix,method='euclidean'){
  AnzTs=ncol(TSMatrix)
  #requireRpackage('TSdist')
  Di=matrix(NaN,AnzTs,AnzTs)
if(sum(!is.finite(TSMatrix))>0)
  warning("Non finite values found. Please use imputeTS::na.interpolation(TSMatrix[,i],option = 'spline') for relevant ith TS or another approach.")

if(!is.matrix(TSMatrix)){
  warning('TSmatrix is not a matrix, Trying to transform...')
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
