DiffFilter=difffilter=function(x,BackTransformation=FALSE){
# differenzenfilter erster ordnung
# dx(i) = x(i)-x(i-1)
# INPUT
# x(1:n)
#
# OUTPUT
# dx(1:n) = x(i)-x(lag(1)), mit  x(-1) = x(1)

# AnzT = length(x)
# LastTime = c(1,1:(AnzT-1))
# dx = x -x[LastTime]
# return(dx)
# #diff(x,lag=1)
  
#see als diffinv
  if(BackTransformation)
    return(diffinv(x[2:length(x)],xi=x[1]))
  else
    return(c(x[1],diff(x,1)))
}