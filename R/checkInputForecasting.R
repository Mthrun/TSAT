checkInputForecasting=function(DataVec, Time, SplitAt, ForecastHorizon, PlotIt) {
  
  if(!is.vector(DataVec)) {
    warning('Input for DataVec is not a vector, parse to vector. If Matrix was given, all columns will be represented as single vector')
    DataVec=as.vector(DataVec)
  }
  
  if(mode(DataVec)!="numeric") {
    warning('DataVec is not a numeric vector. Trying to parse DateVec to numeric')
    DataVec=as.numeric(DataVec)
  } 
  
  if(length(Time) != length(DataVec)) {
    warning('Time and DataVec have differing lengths. Setting Time to 1:length(DataVec)')
    Time = 1:length(DataVec)
  }
  
  if(!inherits(Time, "Date")) {
    if(!all(Time == 1:length(DataVec))) {   # 1:length(DataVec) is the default created if argument is missing (is created in calling function)
      if(!is.character(Time)) {
        warning('Please provide a Time vector consisting of character strings of Date objects. Setting Time to 1:length(DataVec)')
        Time = 1:length(DataVec)
      } else {
        Time = as.Date(Time, 
                tryFormats=c("%y-%m-%d", "%y/%m/%d", "%y %m %d", "%y,%m,%d", "%y;%m;%d", 
                             "%d-%m-%y", "%d/%m/%y", "%d %m %y", "%d,%m,%y", "%d;%m;%y", 
                             "%m-%d-%y", "%m/%d/%y", "%m %d %y", "%m,%d,%y", "%m;%d;%y",
                             "%Y-%m-%d", "%Y/%m/%d", "%Y %m %d", "%Y,%m,%d", "%Y;%m;%d", 
                             "%d-%m-%Y", "%d/%m/%Y", "%d %m %Y", "%d,%m,%Y", "%d;%m;%Y", 
                             "%m-%d-%Y", "%m/%d/%Y", "%m %d %Y", "%m,%d,%Y", "%m;%d;%Y"),
                optional=TRUE)
      }
    }
  }
  if(sum(!is.finite(Time))!=0) {
    warning('Time vector contains NA values or was not possible to convert into a Date object. Setting Time to 1:length(DataVec)')
    Time = 1:length(DataVec)
  }
  
  if(!(is.numeric(SplitAt) && length(SplitAt) == 1 && SplitAt >= 0)) {
    warning('SplitAt should be a positive scalar. Setting SplitAt to length(DataVec)')
    SplitAt = length(DataVec)
  }
  
  if(!(is.numeric(ForecastHorizon) && length(ForecastHorizon) == 1 && ForecastHorizon >= 0)) {
    warning('ForecastHorizon should be a positive scalar. Setting ForecastHorizon to 1')
    ForecastHorizon = 1
  }
  
  if(length(DataVec) < SplitAt) {
    warning('SplitAt is greater then length of DataVec. Setting SplitAt to length(DataVec)')
    SplitAt = length(DataVec)
  }
  
  if(!is.logical(PlotIt)) {
    warning('Input for PlotIt is not logical. Setting PlotIt to FALSE')
    PlotIt = FALSE
  }
  
  return(list("DataVec"=DataVec, "Time"=Time, "SplitAt"=SplitAt, "ForecastHorizon"=ForecastHorizon, "PlotIt"=PlotIt))
}



packageMissingError = function(packageName, n, SplitAt) {
  if (!requireNamespace(packageName,quietly = TRUE)) {
    errorMessage = paste0('Subordinate package (', packageName, ') is missing. No computations are performed.
            Please install the package which is defined in "Suggests".')
    message(errorMessage
    )
    return(errorMessage)
  }
  return(FALSE)
}

