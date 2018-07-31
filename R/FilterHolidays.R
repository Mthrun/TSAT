FilterHolidays=function(Time,Datavector=NULL,HolidaysTime=TSAT::GermanHolidays$Time,PlotIt=FALSE){
  is.Date <- function(x) inherits(x, 'Date')
  
  if(is.Date(Time))
    ind=!Time%in%as.Date(HolidaysTime)
  else
    ind=!Time%in%HolidaysTime
  
  if(!is.null(Datavector)){
    FilteredData=Datavector[ind]
    if(PlotIt){
      plotEvaluationFilteredTS(Time,FilteredData,Datavector,TRUE)
    }
  }else
    FilteredData=NULL
  

  
  return(list(FilteredTime=Time[ind],FilteredData=FilteredData,HolidaysTimes=Time[!ind],HolidaysData=Datavector[!ind]))
}
