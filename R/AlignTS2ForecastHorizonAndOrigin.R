# V = AlignTS2ForecastHorizonAndOrigin(Time, Resolution = "weeks", Origin, Horizon, DataVector)
#
# Description:
# Extends or shortens time series w.r.t to Forecasting
#
# INPUT
# Time               [1:n] vector of time relevant for seq, usually an as.Date object
# Resolution         "weeks", "months", "days" or others accepted strings by seq or "FiskalWeekStartOfMonth"
# Origin             Forecastorigin as indexed w.r.t to Time; Origin=0 means that from the given Time a "future" is generated
# Horizon            positive number, Forecasthorizon as number of steps to be forecasted
# DataVector         Optional, [1:n] vector of data ordered the same way as time
#
# OUTPUT 
# Time	             [1:(Origin+Horizon)] vector
# DataVector	       [1:(Origin+Horizon)] vector
#
# Details: 
# Assumes regular time series with ordered time steps.
# Origin=0 means that a new timestamps in the "future" are generated starting from max(Time)+1
# FiskalWeekStartOfMonth: Means that in a weekly 5,4,4 pattern timestamps of months are generating mapping each start of month to the first fiskal week in that month. It assumes that this pattern is already given in Time and that each week starts on monday with the first week of the year starting on the first monday of that year. Hence, Forecasthorizon is chosen in months.
#
#
# Author: MCT

AlignTS2ForecastHorizonAndOrigin=function(Time,Resolution="weeks",Origin,Horizon,DataVector){
  #months
  n=length(Time)
  if(missing(DataVector))
    DataVector=rep(NaN,length(Time))
  
  n2=length(DataVector)
  if(n2!=n) stop('Length of DataVector is unequal to length of Time')
  
  if(Origin<0) stop('Origin has to be either zero or a positive number')
  if(Horizon<1) stop('Horizon has to be positive number')
  
  #means that first time point is not given in time
  #so we add one time step to horizon and therafter delete one point
  # in the extended time vector
  if(Origin==0){
    Horizon=Horizon+1
  } 
  
  if((Origin+Horizon)>n){
    m=(Origin+Horizon)-n
    
    if(Resolution!="FiskalWeekStartOfMonth"){
      TimeExtended=seq(from=max(Time),length.out=m+1,by=Resolution)[-1]
    }else{#
      requireNamespace("lubridate")
      start = max(Time)#ist immer fiskal-monats anfang
      month = lubridate::month(start)#welcher monat
      
      FiskalPattern = rep(c(5, 4, 4), 12*m)[month:(12*m)]
      MonthPattern=rep(1:12,m)[month:(12*m)]
      
      #MonatsEnde
      for (i in 1:(m+1)) {
        if (i != 1)
          Index = c(Index, Index[i - 1] + FiskalPattern[month + i])
        else
          Index = FiskalPattern[month]
      }
      names(Index)=MonthPattern[1:length(Index)]
      #MonatsAnfang ist auf null
      Index = Index - FiskalPattern[month]
      #Index=Index+1#R startet vector mit 1 nicht mit null
      #print(Index)
      #Index=Index[-1]# Forecastorigin streichen
      AllWeeks = seq(from = start,
                     length.out = 12*m,
                     by = 'weeks')[-1]#R startet vector mit 1 nicht mit null. also ziehen wir eins ab
      #print(AllWeeks[1:30])
      TimeExtended = AllWeeks[Index[1:(m+1)]]#nullter indext wird automatisch geleoscht
    }
   
    DataVectorExtenden=rep(NaN,length(TimeExtended))
    Time=c(Time,TimeExtended)
    DataVector=c(DataVector,DataVectorExtenden)
  }else{
    Time=Time[1:c(Origin+Horizon)]
    DataVector=DataVector[1:c(Origin+Horizon)]
  }
  #means that first time point is not given in time
  if(Origin==0){
    Time=Time[-1]
    DataVector=DataVector[-1]
  }
  return(list(Time=Time,DataVector=DataVector))
}