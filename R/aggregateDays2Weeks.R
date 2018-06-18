aggregateDays2Weeks=function(Time,Datavector,FUN){
  
  requireNamespace('lubridate')
  b=lubridate::isoweek(x = Time)
  c=lubridate::isoyear(x = Time)
  u=unique(c)
  
  if(length(u)==1){
    WeeklyTime=Time[!duplicated(b,fromLast=T)]
    return(list(WeeklyTime=as.Date(WeeklyTime),WeeklyData=tapply(Datavector, b, FUN)))
  }else{
     x=c()
     WeeklyTime=c()
     for(i in u){
       WeeklyTime=c(Time[!duplicated(b[c==i],fromLast=T)])
       x=c(x,tapply(Datavector[c==i], b[c==i], FUN))
     }
     return(list(WeeklyTime=as.Date(WeeklyTime),WeeklyData=x))
   }
             
}