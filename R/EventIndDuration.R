EventIndDuration=TimeDifference=function(Timevector,BeginInd,EndInd,Resolution='mins',Silent=FALSE){
#Calculates the Duration of Event through Indices
#INPUT
#Timevector[1:n]  POSIXlt vecttor of time
#BeginInd[1:m]         Beginning Ind
#EndInd[1:m]           EndInd
#Resolution       Resolution of difference like 'mins','hours','days'
#Silent           Default(TRUE), if FALSE there is no error catching         
#Output
# diff[1:m]            Duration of Event
  
  #Beginn Catching Errors
  if(!Silent){
    if(missing(Timevector)){
      stop('Timevector is missing')
    }
    if(!inherits(Timevector, "POSIXlt")){
      if(inherits(Timevector, "POSIXct")){
        warning('Timevector stores seconds since UNIX epoch (+some other data) and not the character vectors of time. Assuming UTC and trying to transform')
        Timevector=as.POSIXlt(Timevector,tz = 'UTC')
      }else{
        warning(('Timevector is not POSIXlt and not POSIXct, Trying to transform with UTC...'))
        Timevector=as.POSIXlt(Timevector,tz = 'UTC')
      }
    }
    if(missing(BeginInd)|missing(EndInd)){
      stop('BeginInd or EndInd are missing')
    }
    if(!is.vector(BeginInd)|!is.numeric(BeginInd)){
      warning('BeginInd is neither a vector or a numerical value, Trying to transform...')
      BeginInd=as.vector(as.numeric(BeginInd))
    }
    if(!is.vector(EndInd)|!is.numeric(EndInd)){
      warning('EndInd is neither a vector or a numerical value, Trying to transform...')
      EndInd=as.vector(as.numeric(EndInd))
    }
    if(sum(!is.finite(BeginInd))>0){
      indt=which(!is.finite(BeginInd))
      warning(paste('One or more Beginning of Events are missing:',indt,collapse = '\n'))
      # BeginInd=BeginInd[-indt]
      # EndInd=EndInd[-indt]
      return(NaN)
    }
    if(sum(!is.finite(EndInd))>0){
      indt=which(!is.finite(EndInd))
      warning(paste('One or more End of Event are missing:',indt,collapse = '\n'))
     # BeginInd=BeginInd[-indt]
      # EndInd=EndInd[-indt]
      return(NaN)
    }
    if(sum(EndInd-BeginInd<0)>0){
      indt=which(EndInd-BeginInd<0)
      warning(paste('One or more Beginnings are occuring later than Ending:',indt,collapse = '\n'))
    }
    if(sum(EndInd-BeginInd==0)>0){
      indt=which(EndInd-BeginInd==0)
      warning(paste('One or more Beginnings are occuring at the same time than Endings:',indt,collapse = '\n'))
    }
    if(length(BeginInd)!=length(EndInd)){
      warning('Beginning length end Ending length are not the same')
    }
    if(max(EndInd)>length(Timevector)){
      indtempEnd=which(EndInd>length(Timevector))
      warning(paste('End of at least one event is later than the length of Timevector',indtempEnd,collapse = '\n'))
      #return(NaN)
    }
    if(max(BeginInd)>length(Timevector)){
      indtempEndAnfang=which(BeginInd>length(Timevector))
      warning(paste('Beginning of at least one event is later than the length of Timevector',indtempEndAnfang,collapse = '\n'))
      #return(NaN)
    }

  }  #End Catching Errors
  diff=as.numeric(Timevector[EndInd]-Timevector[BeginInd],units=Resolution)
  names(diff)=BeginInd
  if(!Silent){
    if(sum(!is.finite(diff))>0){
      indtemp=which(!is.finite(diff))
      warning(paste('difference of at least one event could not be calculated correctly:',diff[indtemp],indtemp,Timevector[indtemp],Timevector[indtemp],collapse = '\n'))
    }  
    if(sum(diff==0,na.rm = T)>0){
      indtemp=which(diff==0,arr.ind = T)
      warning(paste('difference of at least one event is zero:',indtemp,Timevector[indtemp],Timevector[indtemp],collapse = '\n'))
    }
  }
  return(diff)
}