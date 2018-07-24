LagVector <- function(x, k) {
  if(length(k)!=1) stop('k has to be a scalar number')
  if(!is.numeric(k)) stop('k has to be a numeric value')
  if(class(x)=="Date" |class(x)[1]=="POSIXct" | class(x)[1]=="POSIXt"| class(x)[1]=="POSIXlt") {
    x=as.character(x)
    warning('Returning character vector instead of time.')
  }
  if(!is.vector(x)) stop('x has to be a vector')
  
  N=length(x)
  
  if(k==0) return(x)
  
  k=sign(k)*abs(k)%%N
  
  if(k<0) 
    return(c(tail(x,N+k), rep(NaN, -k)))

  if(k>0)
    return(c(rep(NaN, k), head(x,N-k)))
  
}