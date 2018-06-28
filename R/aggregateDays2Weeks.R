aggregateDays2Weeks=function(Time,Datavector,FUN){
  
  requireNamespace('lubridate')
  b=lubridate::isoweek(x = Time)
  c=lubridate::isoyear(x = Time)
  u=unique(c)
  # WeekTime = seq(as.Date(min(Time)), as.Date(max(Time)
  # ), by ='week'
  # )
  if(length(u)==1){
    WeeklyTime=Time[!duplicated(b,fromLast=T)]
    return(list(WeeklyTime=as.Date(WeeklyTime),WeeklyData=tapply(Datavector, b, FUN)))
  }else{
     x=c()
     WeeklyTime=c()
     for(i in u){
       WeeklyTime=c(Time[!duplicated(b[c==i],fromLast=T)])
       x=c(x,tapply(Datavector[c==i], b[c==i], FUN))
     }
     return(list(WeeklyTime=as.Date(WeeklyTime),WeeklyData=x))
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