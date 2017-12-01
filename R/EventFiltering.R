EventFiltering=function(EventNameorValue,EventArrayOrEvent,Silent=FALSE){
  #  Feature=EventFiltering(EventNameorValue,EventArrayOrEvent)
  #Input
  # EventNameorValue[1]   character or value to search fore
  # EventArray[1:n,1:d]   Array or vector of string or Values where the event has to be searched in
  #Output
  # Feature[1:n]          Numerical Vector with ones and zeros, ones indivate found events
  # author MT 2017
  if(missing(EventArrayOrEvent)) {
    stop('EventArrayOrEvent is missing!')
  }
  if (missing(EventNameorValue)) {
    stop('EventNameorValue is missing!')
  }
  
  if (is.list(EventArrayOrEvent)) {
    stop(
      'EventArrayOrEvent is a list and cannot be interpreted, Please provide a matrix or a vector or a dataframe convertible to a matrix!'
    )
  }
  d=dim(EventArrayOrEvent)[2]
  n=dim(EventArrayOrEvent)[1]
  if (is.null(d) &
      !is.vector(EventArrayOrEvent)) {
    stop('somethin went wrong, length of EventNameorValue not given')
  } else{
    if (is.null(d) & is.vector(EventArrayOrEvent)) {
      d = 1
      n = length(EventArrayOrEvent)
    }
  }
  if (is.null(n))
    stop('somethin went wrong, length of EventNameorValue not given')
  if (n < 2)
    stop('length is only 1!')
  
  if (d == 1 & n > 2) {
    Feature = rep(0, n) #auch bei fehlenden werten tritt kein event auf
    indY = which(EventArrayOrEvent == EventNameorValue)
    if (!Silent & length(indY) == 0) {
      warning('Event could not be found in data.')
    }
    #indN=which(EventArrayOrEvent!=EventNameorValue)
    Feature[indY] = 1
  } else{
    if (!is.matrix(EventArrayOrEvent)) {
      warning('EventArrayOrEvent should be a matrix, trying to transform to a matrix.')
      EventArrayOrEvent = as.matrix(EventArrayOrEvent)
    }
    ArrayData = EventArrayOrEvent
    Tmp = which(EventArrayOrEvent != EventNameorValue, arr.ind = T) #Welche Eintraege haben kein D3 im array
    
    if (is.character(EventArrayOrEvent)) {
      ArrayData[Tmp] = '00'
    } else{
      ArrayData[Tmp] = 0
    }
    for (l in 1:n) {
      #sorgt dafuer das d3 immer vorne steht
      ArrayData[l, ] = sort(ArrayData[l, ], decreasing = T, na.last = T)
    }
    Feature = rep(0, n)
    indY = which(ArrayData[, 1] == EventNameorValue)
    if (!Silent & length(indY) == 0) {
      warning('Event could not be found in data.')
    }
    Feature[indY] = 1
  }
  return(Feature)
}