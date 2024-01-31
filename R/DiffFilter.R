# DiffFilter = DiffFilter(x, BackTransformation=FALSE)
#
# Description:
# 1st Order Differentiation filter
# dx(i) = x(i) - x(i-1)
#
# INPUT
# x(1:n)
#
# OPTIONAL
# BackTransformation
#
# OUTPUT
# dx(1:n) = x(i)-x(lag(1)), with  x(-1) = x(1)
#
# Author: 

DiffFilter = difffilter = function(x, BackTransformation=FALSE) {

# AnzT = length(x)
# LastTime = c(1,1:(AnzT-1))
# dx = x -x[LastTime]
# return(dx)
# #diff(x,lag=1)
  
#see als diffinv
  if(BackTransformation)
    return(diffinv(x[2:length(x)], xi=x[1]))
  else
    return(c(x[1], diff(x,1)))
}
