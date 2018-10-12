ParallelTSlinePlots=function(Data,Time,DailyRegular=TRUE,scales = list(y = "same"),cols,...){
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
      # lubridate::month(min(Time))*30
      # lubridate::day(min(Time))
      ref_date=as.Date(paste0(lubridate::year(min(Time)),'-01-01'))
      first_date=min(Time)
      days=difftime(first_date,ref_date,units = "days")
      
      dat=ts(Data,start =c(lubridate::year(min(Time)),abs(as.numeric(days))),frequency=365)
    }
  }
    
  latticeExtra::horizonplot(dat, scales = list(y = "same"),col.regions=cols,...)
}

