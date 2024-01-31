# GetWorkingDays = GetWorkingDays(Time, HolidaysTime, GermanBridgeDay=TRUE)
#
# Description:
# 
#
# INPUT
# Time                Vector of Time values.
# HolidaysTime
#
# OPTIONAL
# GermanBridgeDay     Boolean, if TRUE adds bridge day holidays. Default is TRUE.
#
# OUTPUT
# DF                  Data frame with Year, Month, WorkingDay, Time
#
# Author: 

GetWorkingDays = function(Time, HolidaysTime, GermanBridgeDay=TRUE,tz="UTC"){
  requireNamespace('lubridate')
  if(!lubridate::is.Date(Time)){
    warning('Time is not a date, calling "as.Date".')
    Time=as.Date(Time,tz=tz)
  }
  years=lubridate::year(Time)
  months=lubridate::month(Time)
  Weekday=lubridate::wday(Time,week_start=1)
  
  if(missing(HolidaysTime)){
    hols=TSAT::GermanHolidays
    hols$Time=as.Date(hols$Time,tz=tz)
    if(GermanBridgeDay){
      HolidayDay=Time %in% hols$Time
    }else{
      HolidayDay=Time %in% hols$Time[hols$Description!="Brueckentag"]
    }
  }else{
    HolidayDay=Time %in% as.Date(HolidaysTime,tz=tz)
  }
  workingday=!HolidayDay & Weekday<6
  
  DF = data.frame(Year=years, Month=months, WorkingDay=workingday, Time=Time)
}