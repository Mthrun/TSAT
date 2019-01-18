aggregateDays2Months=function(Time,Data,FUN,Header,...){
  requireNamespace('tibble')
  requireNamespace('dplyr')
  requireNamespace('lubridate')
  if(!lubridate::is.Date(Time)){
    warning("'Time' is not a date. Calling as.Date()")
    Time=as.Date(Time)
  }
  Dt=tibble::as.tibble(data.frame(TimeTmp=Time,Data=Data))
  
  Monthly =dplyr::group_by(Dt,Time=lubridate::floor_date(TimeTmp,'month')) 
  Monthly = dplyr::summarise(Monthly,Data=FUN(Data,...))

  if(!missing(Header)){
    if(length(Header)==1)
      colnames(Monthly)=c('Time',Header)
    else{
      if(length(Header)==length(colnames(Monthly))){
        colnames(Monthly)=Header
      }else if(length(Header)==(-1+length(colnames(Monthly)))){
        colnames(Monthly)=c('Time',Header)
      }else{
        warning('Length of Header is not matched by number of columns')
        colnames(Monthly)=c('Time',Header)
      }
    }
  }
  return(as.data.frame(Monthly))
}