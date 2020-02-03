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
