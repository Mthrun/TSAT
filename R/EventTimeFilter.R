EventTimeFilter=function(Time,Feature,units='mins',PeriodBetweenEvents=1,formating='(%y-%m-%d %H:%M:%S)',Timezone='UTC',Silent=FALSE){
# V=EventTimeFilter(Time,Feature)
# V=EventTimeFilter(Time,Feature,units='mins',PeriodBetweenEvents=1)
# V=EventTimeFilter(Time,Feature,units,TimeFiltering,PeriodBetweenEvents,formating,Timezone)
# Shut downs flickering in errors or other time series by filling up values in a timeframe defined in PeriodBetweenEvents 
# Version= 0.5.1
# Calculates the 
#
# INPUT:          
# Time[1:n] 			            Time, a chron object or a POSIXlt object or a character vector,  Devicespezifische Zeit, if not chron object, please adjust format neceassry
#                             It hast to be convertible to character
# EventNameorValue[1]         character or value to search fore
# EventFeature[1:n,1]         vector of string or Values where the event has to be searched in
# OPTIONAL:
# units                       char array of units of time for output: "secs", "mins", "hours", "days"
# PeriodBetweenEvents         default=0 => exact computation, else a postive numerical value in time units specified in \code{units}, 
#  							              time betweed two events which is assumed that it interrupts 
# 							              one event into several events (e.g. due to data quality) but is in reality only one event.
# formating                   default='(%y-%m-%d %H:%M:%S)', else check ?strptime function
# Timezone                    sometimes durations and time difference cannot be calculated if timezone is not chosen,
#                             default: 'UTC', , else check ?strptime function
# Silent                      boolean, false, if true, no warnings regarding event computation are given back
# Output
# FilteredEventArray[1:n,1]   
  TimeChar=Time
  if(!inherits(Time, "POSIXlt")){
    if(inherits(Time, "POSIXct")){
      #warning('Timevector stores seconds since UNIX epoch (+some other data) and not the character vectors of time. Assuming UTC and trying to transform')
      TimeChar=as.POSIXlt(Time,tz = 'UTC')
    }else{
      TimeChar=strptime(as.character(Time),format=formating,tz = Timezone) 
    }
  }
  n=length(TimeChar)
  if(length(TimeChar)!=n) stop('Length of Time does not equal number of rows in Feature of length of vector Feature')
  if(!is.numeric(PeriodBetweenEvents)){
    stop('PeriodBetweenEvents has to be a numerical value above zero')
  }
  if(PeriodBetweenEvents<0){
    warning('PeriodBetweenEvents has to be a numerical value above zero. Multiplying with minus!')
    PeriodBetweenEvents=-PeriodBetweenEvents
  }
  indTemp=!is.finite(Feature)
  Temp=Feature[indTemp]
  Feature[indTemp]=0
  Feature2=Feature
  Feature2[Feature>0]=1
  if(PeriodBetweenEvents>0){
    if(n>2){
      LatencyCheck=EventIndDuration(TimeChar,1:(length(Feature2)-1),2:length(Feature2),units,Silent=TRUE)<PeriodBetweenEvents#
      for(k in 1:(length(Feature)-2)){#Das Ende definiert nur der aktuelle Fehler, letztes Event ist sicher vorbei dank Jan
        if(Feature2[k]==1){#Sobald ein Event anliegt
          #if(LatencyCheck[k]&LatencyCheck[k+1])#der Abstand vom anliegen zum naechsten und uebernachsten Event ist kleiner als eine Minute
          if(LatencyCheck[k+1])#der Abstand vom anliegen zum naechsten Event ist kleiner als eine Minute
            if(Feature[k+1]==0)#wenn dort eine falsche null vorliegt, es koennte auch ein anderer wert kommen
              Feature[k+1]=Feature[k] #Dann liegt er auch beim naechsten Zeitstempel noch an, dabei ist esgal ob der uebernaechste
          #Zeitstempel eine null oder eine 1 hat (event anliegt oder nicht)
        }
      }
    }else{
      if(!Silent)
        warning('Length or number of rows of Feature is smaller than 2, cannot calculate approximations.')
    }
  }
  Feature[indTemp]=Temp
  
  return(Feature)
  
}