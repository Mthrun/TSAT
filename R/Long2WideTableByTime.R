Long2WideTableByTime=function(Time,Id,NumericVector,Freq='days'){
  FullTime=seq(from=min(Time),to=max(Time),by=Freq)
 
  u=unique(Id)
  DataFrame=data.frame(Time=FullTime,Data=matrix(NaN,nrow = length(FullTime),ncol = length(u)))
  
  for(i in 1:length(u)){
    ind=which(Id==u[i])
    TimeCur=Time[ind]
    DataCur=NumericVector[ind]
    agg=TSAT::AggregateToUniqueTime(TimeCur,DataCur)
    
    indtime=match(agg$UniqueTime,FullTime)
    DataFrame[indtime,i+1]=agg$Aggregation
  }
  colnames(DataFrame)=c('Time',u)
  return(DataFrame)
}