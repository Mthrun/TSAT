aggregateDays2Weeks=function(Time,Data,FUN,Header,...){
  regular=GenerateRegularDailyTS(TimeChar = Time,Datavec = Data,na.rm = 'zero')
  if(length(Time)!=length(regular$Time)){
    warning("'aggregateDays2Weeks' functions expects a regular time series.
            Generating a Regular timeseries out of 'Time' and 'Data' were missing times are set to zero
            in the 'Data' object. Beware in case of seeting 'FUN=mean'")
    Time=regular$Time
    if(ncol(regular)>2)
        Data=as.matrix(regular[,-which(colnames(regular)=="Time")])
    else
      Data=regular[,-which(colnames(regular)=="Time")]
    
    mode(Data)='numeric'
  }
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
  
  # FirstMonday=min(which(lubridate::wday(Time,week_start=1,label=F)==1))
  # WeekTime=c(seq(from=Time[FirstMonday],by='-7 days',length.out=2)[2],#1Woche davor
  #            seq(from=Time[FirstMonday],by='7 days',to=max(Time)),
  #                seq(from=max(Time),by='7 days',length.out=2)[2])#1woche danach
         
  requireNamespace('tibble')
  if(tibble::is_tibble(Data)){
    requireNamespace('dplyr')
    Time=as.Date(as.matrix(Time))
    #cut requires regular TS!
    Data$WeekTime=cut.Date(Time,breaks='weeks', start.on.monday = TRUE)


    df = dplyr::group_by(Data,WeekTime)
    df2=dplyr::summarize_all(df,sum,na.rm=T)
    
    return(df2)
  }else{#not tibble
    if(missing(Header)){
      if(is.matrix(Data)|is.data.frame(Data)){
        if(length(Time)!=nrow(Data)) stop('Unequal number of rows in Data compared to Time')
        Header=colnames(Data)
      }else{
        Header='Data'
        if(length(Time)!=length(Data)) stop('Unequal length in Data compared to Time')
      }
    }
    DF=data.frame(Time=as.Date(Time),Data,stringsAsFactors = F)
    #cut requires regular TS!
    #requireNamespace('chron')
    DF$Time=cut.Date(DF$Time,breaks='weeks', start.on.monday = TRUE)
    #print(lubridate::wday(lubridate::floor_date(DF$Time, "weeks", week_start = 1)))
    #print(lubridate::wday(DF$Time))
  if(is.vector(Data)){#vector
    dsummary = aggregate(DF$Data ~ DF$Time, data=DF,FUN=FUN,...)
  # DF$Week=lubridate::isoweek(DF$Time)
  # DF$year=lubridate::year(DF$Time)
  # DF$WeekoverYears=paste0(DF$year,DF$Week)
  # dsummary = aggregate(DF$Data ~ DF$WeekoverYears, data=DF,FUN=FUN,...)
  }else{#not vector
    dsummary = aggregate(. ~ DF$Time, data=DF,FUN=FUN,...)
    #aggregation of time itself does not make sense
    dsummary=dsummary[,-2]
  }
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