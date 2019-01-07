GetFullWeeksPerMonth=function(Time,DaysOrWeeks=TRUE){
  
  requireNamespace('lubridate')
  
  if(is.character(Time)) Time=as.Date(Time)

  if(isTRUE(DaysOrWeeks)){#Days
    temp=rep(1,length(Time))
    DF=data.frame(Time=Time,Data=temp)
    WeekTime=aggregateDays2Weeks(DF$Time,DF$Data,sum)$Time
  }else{
    WeekTime=Time
  }
  FullyInMonth=rep(FALSE,length(WeekTime))
  for(day in 1:length(WeekTime)){
    TimeCur=seq(from=WeekTime[day],by='1 days',length.out=7)
    monthNoCur=lubridate::month(TimeCur)
    if(length(unique(monthNoCur))==1) FullyInMonth[day]=TRUE
  }
  month=lubridate::month(WeekTime)
  month[!FullyInMonth]=0
  return(month)
}