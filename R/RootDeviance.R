# V=RootDeviance(x,y,Silent=FALSE)
#
# Description:
# Given two equidistant time series, X and Y, having the same underlying time step set (in particular having the same length), 
# represented by numerical vectors, the sum root distance describes the period-wise square-rooted deviance to the upper and lower
# side of X respectively Y seperated.
#
# INPUT
# x                   [1:n] vector of time series data given by a numerical vector
# y                   [1:n] vector of time series data given by a numerical vector
#
# OUTPUT
# SRD                 SRD(x,y) - Sum of the root errors.
# MRD                 MRD = MRD(x,y) - Mean of the root errors. (SRD / length(x))
# length              sqrt(a^2 + b^2) - Length of the complex number with real part the errors where X is over Y and 
#                     imaginary part the errors where X is under Y.
# bias                bias(x,y) - Is a number bound between -1 and 1. It is 0 iff Y has the same rooted error to the upper 
#                     as it has to the lower side of X. Positivity means Y deviates more to the lower side of X and conversly 
#                     negativity reads as Y deviates more the upper side of X.
# 
#
#author: JM

RootDeviance = function(x,y,Silent=FALSE) {
  if(isFALSE(Silent)){
    if(!is.vector(x) || !is.vector(y))
      warning("Input should be of type vector.")
    if(length(x) != length(y))
      stop("Length of the time series must be equal!")
    if(sum(is.na(x)) > 0 || sum(is.na(y)) > 0)
      stop("Input should not contain NA.")
    if(sum(is.character(x)) > 0 || sum(is.character(y)) > 0)
      stop("No character input allowed.")
  }
  n = length(x)
  a = 0
  b = 0
  for(i in 1:n) {
    e = as.numeric(x[i] - y[i])
    if(e < 0) {
      b = b + sqrt(-e)
    }else {
      a = a + sqrt(e)
    }
  }
  return(list(
    SRD = a + b,
    MRD = (a + b) / n,
    length = sqrt(a^2 + b^2),
    bias = 1 - 4 * atan(b / a) / pi
  ))
}
