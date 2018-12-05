Time2Classification=function(OrderedTimeChar){
  n=length(OrderedTimeChar)
  y=as.character(OrderedTimeChar)
  u=unique(y)
  Cls=rep(NaN,n)
  for(i in 1:length(u)){
    Cls[y==u[i]]=i
  }
  
 
  return(Cls)
}