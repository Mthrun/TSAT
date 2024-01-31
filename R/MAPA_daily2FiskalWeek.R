MAPA_daily2FiskalWeek=function(DailyTime,DailyData,SplitAt,ForecastHorizon=12,ForecastHorizonMonthly=12,FiskalMonthSeason=12,FUN=sum,Confidence=c(0.95),na.rm=TRUE,tz="UTC",...){
  MonthlyData=TSAT::aggregateDays2FiskalMonths(DailyTime,DailyData,FUN = FUN)
    #print(which(MonthlyData$Time=='2018-01-01'))
  if(isTRUE(na.rm)){
    x=MonthlyData$Data
    ind=which(is.finite(x))
    MonthlyData=MonthlyData[ind,]
  }
  #print(which(MonthlyData$Time=='2018-01-01'))
  Time=MonthlyData$Time
  WeeksTime=as.Date(rownames(MonthlyData),tz=tz)
  x=MonthlyData$Data
  n=length(x)
  TrainingTime=head(Time,SplitAt)
  train=head(x,SplitAt)
  test=tail(x,n-SplitAt)
  TestTime=tail(Time,n-SplitAt)
  TestTimeWeeks=tail(WeeksTime,n-SplitAt)
  #print(TestTime)
  #print(TestTimeWeeks)
  out=MAPA::mapa(train,ppy = FiskalMonthSeason,fh = ForecastHorizonMonthly,conf.lvl=Confidence,...)
  Conf=out$PI
  month=lubridate::month(TestTime)
  forecast=out$outfor
  yearpattern=rep(c(5,4,4),length(forecast))
  WeeklyF=c()
  WeeklyData=c()
  WeeklyTestTime=c()
  Lower=c()
  Upper=c()
  for(i in 1:length(forecast)){
    factor=yearpattern[month[i]]
    WeeklyF=c(WeeklyF,rep(forecast[i]/factor,factor))
    WeeklyData=c(WeeklyData,rep(test[i]/factor,factor))
    Lower=c(Lower,rep(Conf[2,i]/factor,factor))
    Upper=c(Upper,rep(Conf[1,i]/factor,factor))
    if(i==1)
      WeeklyTestTime=seq(from=TestTimeWeeks[i],length.out = factor,by = 'weeks')
    else
      WeeklyTestTime=c(WeeklyTestTime,seq(from=TestTimeWeeks[i],length.out = factor,by = 'weeks'))
  }
  DF=data.frame(TestTime=WeeklyTestTime,TestData=WeeklyData,Forecast=WeeklyF)
  Confi=data.frame(TestTime=WeeklyTestTime,Lower=Lower,Upper=Upper)
  return(list(MAPA_F=DF[1:ForecastHorizon,],MAPA_Object=out,Conf=Confi[1:ForecastHorizon,]))
}