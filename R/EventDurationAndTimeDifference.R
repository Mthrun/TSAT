EventDurationAndTimeDifference=function(Time,EventNameorValue,EventArrayOrEvent,units='mins',PeriodBetweenEvents=0,formating='(%y-%m-%d %H:%M:%S)',Timezone='UTC',Silent=FALSE){
  # V=EventDurationAndTimeDifference(Time,EventNameorValue,EventArrayOrEvent)
  # V=EventDurationAndTimeDifference(Time,EventNameorValue,EventArrayOrEvent,units='mins',PeriodBetweenEvents=1)
  # V=EventDurationAndTimeDifference(Time,EventNameorValue,EventArrayOrEvent,units,TimeFiltering,PeriodBetweenEvents,formating,Timezone)
  # Version= 0.5.1
  # Calculates the 
  #
  # INPUT:          
  # Time[1:n] 			      Time, a chron object or a POSIXlt object or a character vector,  Devicespezifische Zeit, if not chron object, please adjust format neceassry
  #                       It hast to be convertible to character
  # EventNameorValue[1]   character or value to search fore
  # EventArray[1:n,1:d]   Array or vector of string or Values where the event has to be searched in
  # OPTIONAL:
  # units                 char array of units of time for output: "secs", "mins", "hours", "days"
  # PeriodBetweenEvents   default=0 => exact computation, else a postive numerical value in time units specified in \code{units}, 
  #  							time betweed two events which is assumed that it interrupts 
  # 							one event into several events (e.g. due to data quality) but is in reality only one event.
  # formating                default='(%y-%m-%d %H:%M:%S)', else check ?strptime function
  # Timezone                sometimes durations and time difference cannot be calculated if timezone is not chosen,
  #                         default: 'UTC', , else check ?strptime function
  # Silent                  boolean, false, if true, no warnings regarding event computation are given back
  # OUTPUT list V with:
  # V$Time[1:n]                 Time as posixCCT
  # V$Count                     Count of Events found
  # V$Duration[1:k]             Duration of Events found
  # V$Difference[1:k-1]       Diference of Time between events found
  # EventIndsFound[1:k]         Indizes for Beginning of Events found (not column depended)
  # 
  # AUTHOR: MT 11/2017
  n=length(EventArrayOrEvent)
  if(n>1){
    Feature=EventFiltering(EventNameorValue,EventArrayOrEvent,Silent=Silent)
  }else{
    warning("length is only 1!") 
    DauerPerDevice=NA
    AbstandPerDevice=NA
    Counts=0
    EventAnfangInd=NULL
    EventEndeInd=NULL
    return(list(Time=Time,Counts=Counts,Duration=DauerPerDevice,Difference=AbstandPerDevice,BeginIndsFound=EventAnfangInd,EndOfEventsInds=EventEndeInd))
  }
  n=length(Feature)
  if(sum(Feature)==0){
    warning('Event could not be found in data.')
    return(list(Time=Time,Counts=0,Duration=NA,Difference=NA,BeginIndsFound=NULL,EndOfEventsInds=NULL))
    
  }
  TimeChar=Time
  if(!inherits(Time, "POSIXlt")){
    if(inherits(Time, "POSIXct")){
      warning('Timevector stores seconds since UNIX epoch (+some other data) and not the character vectors of time. Assuming UTC and trying to transform')
      TimeChar=as.POSIXlt(Time,tz = 'UTC')
    }else{
      TimeChar=strptime(as.character(Time),format=formating,tz = Timezone) 
    }
  }
  if(length(TimeChar)!=n) stop('Length of Time does not equal number of rows in EventArrayOrEvent of length of vector EventArrayOrEvent')
  if(!is.numeric(PeriodBetweenEvents)){
    stop('PeriodBetweenEvents has to be a numerical value above zero')
  }
  if(PeriodBetweenEvents<0){
    warning('PeriodBetweenEvents has to be a numerical value above zero. Multiplying with minus!')
    PeriodBetweenEvents=-PeriodBetweenEvents
  }
  if(PeriodBetweenEvents>0){
    if(n>2){
      LatencyCheck=EventIndDuration(TimeChar,1:(length(Feature)-1),2:length(Feature),units,Silent=TRUE)<PeriodBetweenEvents#
      for(k in 1:(length(Feature)-2)){#Das Ende definiert nur der aktuelle Fehler, letztes Event ist sicher vorbei dank Jan
        if(Feature[k]==1){#Sobald ein Event anliegt
          #if(LatencyCheck[k]&LatencyCheck[k+1])#der Abstand vom anliegen zum naechsten und uebernachsten Event ist kleiner als eine Minute
          if(LatencyCheck[k+1])#der Abstand vom anliegen zum naechsten Event ist kleiner als eine Minute
            Feature[k+1]=1 #Dann liegt er auch beim naechsten Zeitstempel noch an, dabei ist esgal ob der uebernaechste
          #Zeitstempel eine null oder eine 1 hat (event anliegt oder nicht)
        }
      }
    }else{
      warning('Length or number of rows of EventArrayOrEvent is smaller than 2, cannot calculate approximations.')
    }
  }
  
  indevent=which(Feature==1)
  if(length(indevent)>0){
    Vlist=FilterSuccessiveInds(indevent)
    EventAnfangInd=Vlist$values
    EventLaenge=Vlist$lengths
    EventEndeInd=EventAnfangInd+EventLaenge
    indt=which.max(EventEndeInd)
    if(EventEndeInd[indt]>n) EventEndeInd[indt]=NaN #Event dauert bis zum ende der Zeit an
    
    DauerPerDevice=EventIndDuration(TimeChar,EventAnfangInd,EventEndeInd,units,Silent = Silent)
    if(length(EventAnfangInd)>1){
      #letztes Ende und erster Anfang werden geloescht
      AbstandPerDevice=EventIndTimeDifference(TimeChar,EventAnfangInd[-1],EventEndeInd[-length(EventEndeInd)],units,Silent = Silent)
      Counts=length(DauerPerDevice)
    }else{
      AbstandPerDevice=NA
      Counts=1
    }
  }else{
    DauerPerDevice=NA
    AbstandPerDevice=NA
    Counts=0
    EventAnfangInd=NULL
    EventEndeInd=NULL
  }
  
  if(!Silent&sum(DauerPerDevice,na.rm = T)==0)  {
    if(length(DauerPerDevice==0)==Counts&Counts>1){
      warning(paste('Duration is zero'))
      DauerPerDevice=NULL
    }
  }
  Counts=length(DauerPerDevice)  
  if(!Silent& sum(AbstandPerDevice,na.rm = T)==0&Counts>1) warning(paste('Timedifference between two events is zero'))
  
  if(!Silent& Counts==0){
    warning(paste('no event found'))
  }
  if(!Silent&Counts<0|!is.finite(Counts)){ 
    warning('something went wrong')
  }
  
  if(!Silent&sum(!is.finite(DauerPerDevice)>0,na.rm = T)) warning('NaN in Duration found, something went wrong.')
  if(!Silent&sum(!is.finite(AbstandPerDevice)>0,na.rm = T)) warning('NaN in AbstandPerDevice found, something went wrong.')
  
  return(list(Time=TimeChar,Counts=Counts,Duration=DauerPerDevice,Difference=AbstandPerDevice,BeginIndsFound=EventAnfangInd,EndOfEventsInds=EventEndeInd))
}