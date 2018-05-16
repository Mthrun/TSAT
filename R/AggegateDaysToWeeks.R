AggegateDaysToWeeks=function(Data,Time,method='sum'){
  
  Time=as.Date(Time)
  requireNamespace('lubridate')
  
  Years=unique(lubridate::year(as.Date(Time)))
  
  AllWeeks=c()
  for(i in 1:length(Years)){
    ind=which(lubridate::year(Time)==Years[i])
  
    a=Data[ind]
    b=lubridate::isoweek(Time)[ind]
    x=tapply(a, b, sum)
    names(x)=unique(b)
    x=head(x,n = length(x)-1)
    AllWeeks=c(AllWeeks,x)
  }
  
  Time = seq(as.Date(min(Time)), as.Date(max(Time)
  ), by ='week'
  )
  return(list(Weeks=AllWeeks,WeeklyTime=Time))
}