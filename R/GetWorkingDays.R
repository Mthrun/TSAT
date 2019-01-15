GetWorkingDays=function(Time,HolidaysTime,GermanBridgeDay=TRUE){
  requireNamespace('lubridate')
  if(!lubridate::is.Date(Time)){
    warning('Time is not a date, calling "as.Date".')
    Time=as.Date(Time)
  }
  years=lubridate::year(Time)
  months=lubridate::month(Time)
  Weekday=lubridate::wday(Time,week_start=1)
  
  if(missing(HolidaysTime)){
    hols=TSAT::GermanHolidays
    hols$Time=as.Date(hols$Time)
    if(GermanBridgeDay){
      HolidayDay=Time %in% hols$Time
    }else{
      HolidayDay=Time %in% hols$Time[hols$Description!="Brueckentag"]
    }
  }else{
    HolidayDay=Time %in% as.Date(HolidaysTime)
  }
  workingday=!HolidayDay & Weekday<6
  
  DF=data.frame(Year=years,Month=months,WorkingDay=workingday,Time=Time)
}