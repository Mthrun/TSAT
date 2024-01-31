# V=FilterHolidays(Time, Datavector=NULL,HolidaysTime = TSAT::GermanHolidays$Time,PlotIt=FALSE)
#
# DESCRIPTION
# Filters Holidays out of daily data
#
# INPUT
# Time                  [1:n] vector of time, as.Date or POSIXct objects are accepted
# OPTIONAL
# Datavector            [1:n] vector of values or a Key in case of a data frame. Default is NULL
# HolidaysTime          [1:k] vector of time with k holidays, as.Date or POSIXct objects are accepted. 
#                       Default are german holidays, see TSAT::GermanHolidays$Time
# PlotIt                TRUE: Evaluates output of function versus input by plots, works only if Datavector given.
#                       Default is FALSE
#
# OUTPUT
# FilteredTime         [1:m] vector with time objects of the m=n-k dates, which are not holidays
# FilteredData         [1:m] vector with data corresponding to the m=n-k dates, which are not holidays
# HolidaysTimes        [1:k] vector with time objects of the k holidays
# HolidaysData         [1:k] vector with data corresponding to the k holidays
# where k,m<=n and n=m+k
#
# author MCT

FilterHolidays=function(Time,Datavector=NULL,HolidaysTime=TSAT::GermanHolidays$Time,PlotIt=FALSE,tz="UTC"){
  is.Date <- function(x) inherits(x, 'Date')
  
  if(is.Date(Time))
    ind=!Time%in%as.Date(HolidaysTime,tz=tz)
  else
    ind=!Time%in%HolidaysTime
  
  if(!is.null(Datavector)){
    FilteredData=Datavector[ind]
    if(PlotIt){
      dataAfter = Datavector
      dataAfter[!ind] = NaN
      plotEvaluationFilteredTS(Time,Datavector,dataAfter,TRUE)
    }
  }else
    FilteredData=NULL
  

  
  return(list(FilteredTime=Time[ind],FilteredData=FilteredData,HolidaysTimes=Time[!ind],HolidaysData=Datavector[!ind]))
}
