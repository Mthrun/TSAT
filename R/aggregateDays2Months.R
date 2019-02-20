aggregateDays2Months=function(Time,Data,FUN,Header,Period="month",...){
  requireNamespace('tibble')
  requireNamespace('dplyr')
  requireNamespace('lubridate')
  if(!lubridate::is.Date(Time)){
    warning("'Time' is not a date. Calling as.Date()")
    Time=as.Date(Time)
  }

  if(is.vector(Data)){
    Boolean=TRUE
  }else if(!is.null(ncol(Data))){
    if(ncol(Data)==1){
      Boolean=TRUE
    }else{
      Boolean=FALSE
    }
  }else{
    Boolean=FALSE
  }
  if(isTRUE(Boolean)){
    xx=data.frame(Time,Data)
    colnames(xx)=c('TimeTmp','Data')
  Dt=tibble::as.tibble(xx)
  
  Monthly =dplyr::group_by(Dt,Time=lubridate::floor_date(TimeTmp,Period)) 
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
  }else{
    Monthly=c()
    DateTemp=Data
    for(i in 1:ncol(DateTemp)){
      if(i==1)
        Monthly=as.data.frame(aggregateDays2Months(Time=Time,Data = DateTemp[,i],FUN=FUN,Header=Header,...))
      else
        Monthly=cbind(Monthly,as.data.frame(aggregateDays2Months(Time=Time,Data = DateTemp[,i],FUN=FUN,Header=Header,...))$Data)
    }
    if(!is.null(colnames(DateTemp))){
      colnames(Monthly)=c('Time',colnames(DateTemp))
    }else{
      colnames(Monthly)=c('Time',paste0('C',1:ncol(DateTemp)))
    }
    return(Monthly)
  }
  
}