ConvertPOSIXlist2Vector=function(POSIXlist){
  n=length(POSIXlist)
  x= as.POSIXlt(as.Date(vector(mode="numeric",n),origin = "1970-01-01"))
  for(i in 1:n){
    x[i]=POSIXlist[i]
  }
  return(x)
}
