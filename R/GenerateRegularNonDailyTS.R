GenerateRegularNonDailyTS=function(Datavec,TimeChar,TimeResolutionBegin='secs',TimeResolutionEnd='mins',Minutes=1,PlotIt=FALSE,formating='(%y-%m-%d %H:%M:%S)',tz = 'UTC'){
  
  #TimeChar=as.POSIXct(Time,origin = '1970-01-01',tz = 'UTC')
  #author: MT 02/2018
  requireNamespace('zoo')
  requireNamespace('xts')
  # if(!inherits(TimeChar, "POSIXlt")){
  #   if(inherits(TimeChar, "POSIXct")){
  #     warning('Timevector stores seconds since UNIX epoch (+some other data) and not the character vectors of time. Assuming UTC and trying to transform')
  #     TimeChar=as.POSIXlt(TimeChar,tz = tz)
  #   }else{
  #     TimeChar=strptime(as.character(TimeChar),format=formating,tz = tz) 
  #   }
  # }
  if(TimeResolutionBegin=='days') warning('Please use the "GenerateRegularDailyTS" function.')
  ind=!duplicated(TimeChar)
  outage.zoo <- zoo::as.zoo(x = Datavec[ind], order.by=TimeChar[ind])
  
  full=merge(outage.zoo, zoo::zoo(, seq(as.POSIXct(start(outage.zoo),format=formating,tz=tz), as.POSIXct(end(outage.zoo),format=formating,tz=tz), 
                                   by = TimeResolutionBegin)), all = T)
  
  TS1=zoo::na.approx(full)
  #frequency(TS1)=1
  
  TStrans=xts::to.period(TS1,period=TimeResolutionEnd,k=Minutes,drop.time=F,OHLC = FALSE)
if(PlotIt){
  #plot(na.spline(full),col='red')
  m <-
    graphics::layout(matrix(c(1, 1, 2,2,3,3)))
  plot(TimeChar,Datavec,col='blue',main='Irregular Time Series')
  
  plot(TStrans,pch=4,col='red',type='p',main='Regular Time Series, TimeResolutionEnd')
  plot(TS1,col='black',type='p',pch=1, main='Regular Time Series, TimeResolutionBegin')
  
}
    return(TStrans)
}
