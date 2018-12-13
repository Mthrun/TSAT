aggregateDays2Weeks=function(Time,Data,FUN,Header,...){
  
  # requireNamespace('lubridate')
  # b=lubridate::isoweek(x = Time)
  # c=lubridate::isoyear(x = Time)
  # u=unique(c)
  # # WeekTime = seq(as.Date(min(Time)), as.Date(max(Time)
  # # ), by ='week'
  # # )
  # if(length(u)==1){
  #   WeeklyTime=Time[!duplicated(b,fromLast=T)]
  #   WeeklyData=tapply(Datavector, b, FUN)
  # }else{
  #   WeeklyData=c()
  #   # WeeklyTime=NULL
  #   WeeklyTime=Time[!duplicated(b[c==u[1]],fromLast=T)]#why does that work?
  #    for(i in u){
  #      # if(is.null(WeeklyTime))
  #      # 
  #      # else
  #      #   WeeklyTime=c(WeeklyTime,Time[!duplicated(b[c==i],fromLast=T)])
  #      
  #      WeeklyData=c(WeeklyData,tapply(Datavector[c==i], b[c==i], FUN))
  #    }
  #   
  #  }
  # WeeklyTime=as.Date(WeeklyTime)
  # if(length(WeeklyTime)!=length(WeeklyData))
  #   warning('Length is not equal. please transform manually')
  # return(list(WeeklyTime=WeeklyTime,WeeklyData=WeeklyData))     
  
  requireNamespace('tibble')
  if(tibble::is.tibble(Data)){
    requireNamespace('dplyr')
    Time=as.Date(as.matrix(Time))
    Data$WeekTime=cut(Time,breaks='weeks')
    
    df = dplyr::group_by(Data,WeekTime)
    df2=dplyr::summarize_all(df,sum,na.rm=T)
    
    return(df2)
  }else{
    if(missing(Header)){
      if(is.matrix(Data)|is.data.frame(Data)){
        if(length(Time)!=nrow(Data)) stop('Unequal number of rows in Data compared to Time')
        Header=colnames(Data)
      }else{
        Header='Data'
        if(length(Time)!=length(Data)) stop('Unequal length in Data compared to Time')
      }
    }
  if(!is.vector(Data)) warning('May not Work with more than one column of data')
 
  DF=data.frame(Time=Time,Data=Data)
  DF$Time=cut(Time,breaks='weeks')
  
  dsummary = aggregate(DF$Data ~ DF$Time, data=DF,FUN=FUN,...)

  dsummary$'DF$Time'=as.Date(dsummary$'DF$Time')
  
  if(length(Header)==1)
    colnames(dsummary)=c('Time',Header)
  else{
    if(length(Header)==length(colnames(dsummary))){
      colnames(dsummary)=Header
    }else if(length(Header)==(-1+length(colnames(dsummary)))){
      colnames(dsummary)=c('Time',Header)
    }else{
      warning('Length of Header is not matched by number of columns')
      colnames(dsummary)=c('Time',Header)
    }
  }

  return(dsummary)
  }
}

# AggegateDaysToWeeks=function(Data,Time,method='sum'){
#   
#   Time=as.Date(Time)
#   requireNamespace('lubridate')
#   
#   Years=unique(lubridate::year(as.Date(Time)))
#   
#   AllWeeks=c()
#   for(i in 1:length(Years)){
#     ind=which(lubridate::year(Time)==Years[i])
#     
#     a=Data[ind]
#     b=lubridate::isoweek(Time)[ind]
#     x=tapply(a, b, sum)
#     names(x)=unique(b)
#     x=head(x,n = length(x)-1)
#     AllWeeks=c(AllWeeks,x)
#   }
#   
#   Time = seq(as.Date(min(Time)), as.Date(max(Time)
#   ), by ='week'
#   )
#   return(list(Weeks=AllWeeks,WeeklyTime=Time))
# }