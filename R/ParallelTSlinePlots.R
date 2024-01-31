# ParallelTSlinePlots(Data, Time, DailyRegular = TRUE, scales = list(y = "same"), cols, ...)
#
# DESCRIPTION:
# Plots many time series in parallel from a matrix of Data.
# It is a shortcut wrapper for latticeExtra::horizonplot in order to omit the manual creation of ts objects
#
# INPUT
# Data               [1:n,1:d] d mulitvariate timeseries of n cases
# OPTIONAL
# Time               Only if time series are regular, if not please do not set
# DailyRegular       If TRUE: time series is regular with as.date objects, in other case its regular as defined by Time
#                    Default is TRUE
# scales             Same is best variant. It is preferred to normalize data before using this function. 
#                    Please see latticeExtra::horizonplot for other options. Default is "same"
# cols               Color scale as character vector. Default is DataVisualizations::HeatmapColors sequence
# ...                Arguments passed on to latticeExtra::horizonplot
#
#
# DETAILS:
# If DailyRegular=FALSE, then the x axis will be in UNIX seconds since 1970-01-01.
#
#
# Author: MCT

ParallelTSlinePlots=function(Data,Time,DailyRegular=TRUE,scales = list(y = "same"),cols,tz="UTC",...){
  requireNamespace('latticeExtra')
  if(!is.matrix(Data)) Data=as.matrix(Data)
  if(!mode(Data)=='numeric') mode(Data)='numeric'
  
  if(missing(cols)){
    requireNamespace('DataVisualizations')
    cols=DataVisualizations::HeatmapColors
  }
  if(missing(Time)){
    dat=ts(Data)
  }else{
    if(!DailyRegular){
      dat=ts(Data,frequency = 1,start = min(Time),end = max(Time))
    }else{
      requireNamespace('lubridate')
      requireNamespace('tibble')
      # lubridate::month(min(Time))*30
      # lubridate::day(min(Time))
      if(tibble::is.tibble(Time)){
        Time = as.Date(as.matrix(Time),tz=tz)
      }
      ref_date=as.Date(paste0(lubridate::year(min(Time)),'-01-01'),tz=tz)
      first_date=min(Time)
      days=difftime(first_date,ref_date,units = "days")
      
      dat=ts(Data,start =c(lubridate::year(min(Time)),abs(as.numeric(days))),frequency=365)
    }
  }
  #latticeExtra::horizonplot(dat, scales = list(y = "same"),col.regions=cols,...)
  latticeExtra::horizonplot(dat, scales = scales,col.regions=cols,...)
}

