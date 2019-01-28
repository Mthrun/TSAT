aggregateTime2Days=function(Time,Data,FUN,Header,...){
  
  if(missing(Header)){
    if(is.matrix(Data)|is.data.frame(Data)){
      if(length(Time)!=nrow(Data)) stop('Unequal number of rows in Data compared to Time')
      Header=colnames(Data)
      if(ncol(Data)>1) warning('May not Work with more than one column of data')
    }else{
    Header='Data'
    if(length(Time)!=length(Data)) stop('Unequal length in Data compared to Time')
    }
  }

  DF=data.frame(Time=Time,Data=Data)
  DF$Time=cut(Time,breaks='days')
  
  dsummary = aggregate(DF$Data ~ DF$Time, FUN=FUN,    data=DF,...)

  colnames(dsummary)=c('Time',Header)

  dsummary$Time=as.Date(dsummary$Time)
  return(dsummary)
}